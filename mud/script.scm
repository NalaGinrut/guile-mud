;;; MUD Script
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

(define-module (mud script)
  #:use-module (mud request)
  #:use-module (mud colorized)
  #:export (mud-info-update get-default-instr-response
            get-default-wrong-response))

(define *all-clients* (make-hash-table))
(define (get-status client)
  (hash-ref *all-clients* client))
(define (update-status! client status)
  (hash-set! *all-clients* client status))

(define *default-news* 
  "现在周围没啥鸟事，你在想哪里有妞可泡...
")

(define *greeting*
  (format #f
          "欢迎来到~a，这位勇士看上去很~a啊！你如果不知道该做什么，可以试试~a~%"
          (->location "华强北") (->motion "猥琐") (->op "help")))

(define (get-default-instr-response instr)
  (let ((op (mud-instr-op instr))
        (target (mud-instr-target instr)))
    (format #f "你找不到任何~a来~a~%，周围的人都很奇怪地看着你...~%"
            (->op op) (->item target))))

(define (get-default-wrong-response)
  (format #f
          "这位勇士你好像在~a~a了~a，周围的人都奇怪地看着你...~%"
          (->location "华强北") (->op "失落") (->item "智商")))

;; TODO: Should multicase the latest news
(define (print-news port)
  (display *default-news* port))

(define (mud-info-update client)
  (let ((status (get-status client))
        (port (car client)))
    (set-port-encoding! port "UTF-8")
    (cond
     ((not status)
      (display *greeting* port)
      (force-output port)
      (update-status! client 'welcomed))
     ((eqv? status 'welcomed)
      (print-news port))
     (else (error mud-info-update "invalid status for client!" status)))))
