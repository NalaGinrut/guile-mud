;;; MUD Request
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

(define-module (mud request)
  #:use-module (mud colorized)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-9)
  #:export (read-request mud-instr-op mud-instr-target
            mud-instruction? make-request help? show-help))

(define-record-type <mud-instruction>
  (make-mud-instruction op target)
  mud-instruction?
  (op mud-instr-op)
  (target mud-instr-target))

(define (read-request port)
  (parse-line-cmd port))   

(define (trim str)
  (string-trim-both str (lambda (c) (member c '(#\nl #\return #\sp)))))

(define (parse-simple-instruction line)
  (let ((ll (string-split (trim line) #\sp)))
    (if (< (length ll) 2)
        (if (string=? (car ll) "help")
            'help
            ((@ (mud response) generate-default-response)))
        (make-mud-instruction (->op (trim (car ll))) (->item (trim (cadr ll)))))))

(define (parse-line-cmd port)
  (let ((line (read-line port)))
    ;; TODO: try complex and extensable instruction
    (parse-simple-instruction line)))

(define (help? r)
  (display r)(newline)
  (eqv? r 'help))

(define (show-help)
  (format #f "最简单的操作为\"op item\"，如：你可以输入\"~a ~a\"~%"
          (->op "吃") (->item "屎")))
