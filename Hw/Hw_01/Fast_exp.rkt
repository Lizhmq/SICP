#lang racket

(define (square x) (* x x))
(define (exp a n)
  (define (solve a n pro)
    (if (=  n 0)
        pro
        (if (= (remainder n 2) 0)
            (solve (* a a) (/ n 2) pro)
            (solve a (- n 1) (* pro a)))))
  (solve a n 1))
(define (work)
  (let ((a (read)) (n (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (exp a n)) (work)))))
(work)