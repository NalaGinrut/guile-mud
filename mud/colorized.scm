;;; MUD Colorized
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

(define-module (mud colorized)
  #:use-module (ice-9 rdelim)
  #:use-module ((srfi srfi-1) #:select (filter-map any proper-list?))
  #:use-module (srfi srfi-9)
  #:export (color-it colorize-it colorize color-func colorize-string 
            colorized-display ->location ->op ->item ->people ->motion))

;; color-scheme context, contains some info to be used
(define-record-type color-scheme
  (make-color-scheme obj type color control method)
  color-scheme?
  (obj color-scheme-obj) ; the obj to be colored
  (type color-scheme-type) ; the obj type (for debug/test)
  (color color-scheme-color) ; the color
  (control color-scheme-control) ; ansi control code
  (method color-scheme-method)) ; colorized method for the obj type

(define *color-list*
  `((CLEAR       .   "0")
    (RESET       .   "0")
    (BOLD        .   "1")
    (DARK        .   "2")
    (UNDERLINE   .   "4")
    (UNDERSCORE  .   "4")
    (BLINK       .   "5")
    (REVERSE     .   "6")
    (CONCEALED   .   "8")
    (BLACK       .  "30")
    (RED         .  "31")
    (GREEN       .  "32")
    (YELLOW      .  "33")
    (BLUE        .  "34")
    (MAGENTA     .  "35")
    (CYAN        .  "36")
    (WHITE       .  "37")
    (ON-BLACK    .  "40")
    (ON-RED      .  "41")
    (ON-GREEN    .  "42")
    (ON-YELLOW   .  "43")
    (ON-BLUE     .  "44")
    (ON-MAGENTA  .  "45")
    (ON-CYAN     .  "46")
    (ON-WHITE    .  "47")))

(define (get-color color)
  (assq-ref *color-list* color))

(define (generate-color colors)
  (let ((color-list
         (filter-map get-color colors)))
    (if (null? color-list)
        ""
        (string-append "\x1b[" (string-join color-list ";" 'infix) "m"))))

(define (colorize-string-helper color str control)
  (string-append (generate-color color) str (generate-color control)))

;; test-helper functions
;; when eanbled, it won't output colored result, but just normal.
;; it used to test the array/list/vector print result.
(define color-func (make-parameter colorize-string-helper))

(define (color-it cs) 
  (let* ((obj (color-scheme-obj cs))
         (str (object->string obj))
         (color (color-scheme-color cs))
         (control (color-scheme-control cs)))
    (color-it-inner color str control)))

(define (color-it-inner color str control)
  ((color-func) color str control))

(define* (space #:optional (port (current-output-port)))
  (display #\sp port))

(define (colorize-string str color)
  "Example: (colorize-string \"hello\" '(BLUE BOLD))" 
  (and (not (list? color)) (error colorize-string "color should be a list!" color))
  (colorize-string-helper color str '(RESET)))

(define (colorized-display str color)
  "Example: (colorized-display \"hello\" '(BLUE BOLD))"
  (display (colorize-string str color)))

(define* (colorize-it obj #:optional (port (current-output-port)))
  (colorize obj port)
  (newline port))

(define (->cstr obj)
  (call-with-output-string
   (lambda (port)
     (colorize obj port))))

(define-syntax-rule (->location str)
  (colorize-string str '(YELLOW)))

(define-syntax-rule (->motion str)
  (colorize-string str '(MAGENTA)))

(define-syntax-rule (->people str)
  (colorize-string str '(CYAN)))

(define-syntax-rule (->item str)
  (colorize-string str '(BLUE)))

(define-syntax-rule (->op str)
  (colorize-string str '(RED)))
