;;; MUD Server
;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (mud server mud)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (mud request)
  #:use-module (mud response)
  #:use-module (mud script)
  #:use-module (mud server)
  #:use-module (ice-9 poll))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

(define-record-type <mud-server>
  (make-mud-server socket poll-idx poll-set)
  mud-server?
  (socket mud-socket)
  (poll-idx mud-poll-idx set-mud-poll-idx!)
  (poll-set mud-poll-set))

(define *error-events* (logior POLLHUP POLLERR))
(define *read-events* POLLIN)
(define *events* (logior *error-events* *read-events*))

;; -> server
(define* (mud-open #:key
                    (host #f)
                    (family AF_INET)
                    (addr (if host
                              (inet-pton family host)
                              INADDR_LOOPBACK))
                    (port 8080)
                    (backlog 128)
                    (socket (make-default-socket family addr port)))
  (listen socket backlog)
  (sigaction SIGPIPE SIG_IGN)
  (let ((poll-set (make-empty-poll-set)))
    (poll-set-add! poll-set socket *events*)
    (make-mud-server socket 0 poll-set)))

;; -> (client request body | #f #f #f)
(define (mud-read server)
  (let* ((poll-set (mud-poll-set server)))
    (let lp ((idx (mud-poll-idx server)))
      (let ((revents (poll-set-revents poll-set idx)))
        (cond
         ((zero? idx)
          ;; The server socket, and the end of our downward loop.
          (cond
           ((zero? revents)
            ;; No client ready, and no error; poll and loop.
            (poll poll-set)
            (lp (1- (poll-set-nfds poll-set))))
           ((not (zero? (logand revents *error-events*)))
            ;; An error.
            (set-mud-poll-idx! server idx)
            (throw 'interrupt))
           (else
            ;; A new client. Add to set, poll, and loop.
            ;;
            ;; FIXME: preserve meta-info.
            (let ((client (accept (poll-set-port poll-set idx))))
              ;; Buffer input and output on this port.
              (setvbuf (car client) _IOFBF)
              ;; From "HOP, A Fast Server for the Diffuse Web", Serrano.
              (setsockopt (car client) SOL_SOCKET SO_SNDBUF (* 12 1024))
              (mud-info-update client)
              (poll-set-add! poll-set (car client) *events*)
              (poll poll-set)
              (lp (1- (poll-set-nfds poll-set)))))))
         ((zero? revents)
          ;; Nothing on this port.
          (lp (1- idx)))
         ;; Otherwise, a client socket with some activity on
         ;; it. Remove it from the poll set.
         (else
          (let ((port (poll-set-remove! poll-set idx)))
            ;; Record the next index in all cases, in case the EOF check
            ;; throws an error.
            (set-mud-poll-idx! server (1- idx))
            (cond
             ((eof-object? (peek-char port))
              ;; EOF.
              (close-port port)
              (lp (1- idx)))
             (else
              ;; Otherwise, try to read a request from this port.
              (with-throw-handler
               #t
               (lambda ()
                 (let ((req (read-request port)))
                   (values port
                           req
                           #f))) ; we don't need body
               (lambda (k . args)
                 (define-syntax-rule (cleanup-catch statement)
                   (catch #t
                     (lambda () statement)
                     (lambda (k . args)
                       (format (current-error-port) "In ~a:\n" 'statement)
                       (print-exception (current-error-port) #f k args))))
                 (cleanup-catch (close-port port)))))))))))))

;; -> 0 values
;; For current MUD design, we don't need any headers
(define (mud-write server client response body)
  ;;(set-port-encoding! client "iso8859-1")
  (write-response response client)
  (force-output client)
  ;; MUD always need long connection
  (poll-set-add! (mud-poll-set server) client *events*)
  (values))

;; -> unspecified values
(define (mud-close server)
  (let ((poll-set (mud-poll-set server)))
    (let lp ((n (poll-set-nfds poll-set)))
      (if (positive? n)
          (begin
            (close-port (poll-set-remove! poll-set (1- n)))
            (lp (1- n)))))))

(define-server-impl mud
  mud-open
  mud-read
  mud-write
  mud-close)
