#! /usr/local/guile \
-L ./
!#

(setlocale LC_ALL "zh_CN.UTF-8")
(use-modules (mud server) (mud request))

;; we don't need body
(define (try-mud r b)
  (cond
   ((mud-instruction? r)
    (let ((op (mud-instr-op r))
          (target (mud-instr-target r)))
      (values
       (format #f "你~a了个~a~%" op target)
       #f)))
   ((help? r)
    (values (show-help) #f))
   (else (values r #f))))
    
(run-server try-mud 'mud `(#:port 1234))
