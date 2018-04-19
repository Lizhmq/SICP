#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
(define (count-pairs pair)
   (define (set-to-lzzz a)
     (set-car! a (cons 'lzzz (car a))))
   (define (check a)
     (and (pair? a) (pair? (car a)) (eq? 'lzzz (caar a))))
   (cond ((not (pair? pair)) 0)
         ((check pair) 0)
         (else
          (let ((a (car pair)) (b (cdr pair)))
            (begin (set-to-lzzz pair)
                   (+ (count-pairs a)
                      (count-pairs b)
                      1))))))
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