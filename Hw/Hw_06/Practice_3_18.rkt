#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
(define (check-cycle lst)
  (define (tag pair)
    (set-car! pair (cons 'lzzz (car pair))))
  (if (pair? lst)
      (if (and (pair? (car lst)) (eq? (caar lst) 'lzzz))
          #t
          (begin (tag lst) (check-cycle (cdr lst))))
      #f))
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)