;;; Web Server
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

(define-module (mud server)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (mud request)
  #:use-module (mud response)
  #:use-module (system repl error-handling)
  #:use-module (ice-9 control)
  #:use-module (ice-9 iconv)
  #:export (define-server-impl
            lookup-server-impl
            open-server
            read-client
            handle-request
            sanitize-response
            write-client
            close-server
            serve-one-client
            run-server))

(define *timer* (gettimeofday))
(define (print-elapsed who)
  (let ((t (gettimeofday)))
    (pk who (+ (* (- (car t) (car *timer*)) 1000000)
               (- (cdr t) (cdr *timer*))))
    (set! *timer* t)))

(eval-when (expand)
  (define *time-debug?* #f))

(define-syntax debug-elapsed
  (lambda (x)
    (syntax-case x ()
      ((_ who)
       (if *time-debug?*
           #'(print-elapsed who)
           #'*unspecified*)))))

(define-record-type server-impl
  (make-server-impl name open read write close)
  server-impl?
  (name server-impl-name)
  (open server-impl-open)
  (read server-impl-read)
  (write server-impl-write)
  (close server-impl-close))

(define-syntax-rule (define-server-impl name open read write close)
  (define name
    (make-server-impl 'name open read write close)))

(define (lookup-server-impl impl)
  "Look up a server implementation.  If IMPL is a server
implementation already, it is returned directly.  If it is a symbol, the
binding named IMPL in the ‘(web server IMPL)’ module is
looked up.  Otherwise an error is signaled.

Currently a server implementation is a somewhat opaque type, useful only
for passing to other procedures in this module, like
‘read-client’."
  (cond
   ((server-impl? impl) impl)
   ((symbol? impl)
    (let ((impl (module-ref (resolve-module `(mud server ,impl)) impl)))
      (if (server-impl? impl)
          impl
          (error "expected a server impl in module" `(mud server ,impl)))))
   (else
    (error "expected a server-impl or a symbol" impl))))

;; -> server
(define (open-server impl open-params)
  "Open a server for the given implementation.  Return one value, the
new server object.  The implementation's ‘open’ procedure is
applied to OPEN-PARAMS, which should be a list."
  (apply (server-impl-open impl) open-params))

;; -> (client request body | #f #f #f)
(define (read-client impl server)
  "Read a new client from SERVER, by applying the implementation's
‘read’ procedure to the server.  If successful, return three
values: an object corresponding to the client, a request object, and the
request body.  If any exception occurs, return ‘#f’ for all three
values."
  (call-with-error-handling
   (lambda ()
     ((server-impl-read impl) server))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error (lambda _ (values #f #f #f))))

;; -> response body
;; -> response body state
(define (handle-request handler request body state)
  "Handle a given request, returning the response and body.

The response and response body are produced by calling the given
HANDLER with REQUEST and BODY as arguments.

The elements of STATE are also passed to HANDLER as
arguments, and may be returned as additional values.  The new
STATE, collected from the HANDLER's return values, is then
returned as a list.  The idea is that a server loop receives a handler
from the user, along with whatever state values the user is interested
in, allowing the user's handler to explicitly manage its state."
  (call-with-error-handling
   (lambda ()
     (call-with-values (lambda ()
                         (with-stack-and-prompt
                          (lambda ()
                            (apply handler request body state))))
       (lambda (response body . state)
         (debug-elapsed 'handler)
         (values response body)
         (debug-elapsed 'sanitize)
         (values response body state))))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error (lambda _
                  (values (generate-default-response request) #f state))))

;; -> unspecified values
(define (write-client impl server client response body)
  "Write an HTTP response and body to CLIENT.  If the server and
client support persistent connections, it is the implementation's
responsibility to keep track of the client thereafter, presumably by
attaching it to the SERVER argument somehow."
  (call-with-error-handling
   (lambda ()
     ((server-impl-write impl) server client response body))
   #:pass-keys '(quit interrupt)
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error (lambda _ (values))))

;; -> unspecified values
(define (close-server impl server)
  "Release resources allocated by a previous invocation of
‘open-server’."
  ((server-impl-close impl) server))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

(define (with-stack-and-prompt thunk)
  (call-with-prompt (default-prompt-tag)
                    (lambda () (start-stack #t (thunk)))
                    (lambda (k proc)
                      (with-stack-and-prompt (lambda () (proc k))))))
  
;; -> new-state
(define (serve-one-client handler impl server state)
  "Read one request from SERVER, call HANDLER on the request
and body, and write the response to the client.  Return the new state
produced by the handler procedure."
  (debug-elapsed 'serve-again)
  (call-with-values
      (lambda ()
        (read-client impl server))
    (lambda (client request body)
      (debug-elapsed 'read-client)
      (if client
          (call-with-values
              (lambda ()
                (handle-request handler request body state))
            (lambda (response body state)
              (debug-elapsed 'handle-request)
              (write-client impl server client response body)
              (debug-elapsed 'write-client)
              state))
          state))))

(define* (run-server handler #:optional (impl 'http) (open-params '())
                     . state)
  "Run Guile's built-in web server.

HANDLER should be a procedure that takes two or more arguments,
the HTTP request and request body, and returns two or more values, the
response and response body.

For example, here is a simple \"Hello, World!\" server:

@example
 (define (handler request body)
   (values '((content-type . (text/plain)))
           \"Hello, World!\"))
 (run-server handler)
@end example

The response and body will be run through ‘sanitize-response’
before sending back to the client.

Additional arguments to HANDLER are taken from
STATE.  Additional return values are accumulated into a new
STATE, which will be used for subsequent requests.  In this way a
handler can explicitly manage its state.

The default server implementation is ‘http’, which accepts
OPEN-PARAMS like ‘(#:port 8081)’, among others.  See \"Web
Server\" in the manual, for more information."
  (let* ((impl (lookup-server-impl impl))
         (server (open-server impl open-params)))
    (call-with-sigint
     (lambda ()
       (let lp ((state state))
         (lp (serve-one-client handler impl server state))))
     (lambda ()
       (close-server impl server)
       (values)))))
