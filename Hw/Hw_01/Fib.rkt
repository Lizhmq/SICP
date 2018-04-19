#lang racket

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (f e d c b a)
  (+ a (* 4 b) (* 5 c) (- (* 2 (square d))) (cube e)))
(define (solve n)
  (define (h a b c d e n)
    (if (< n 5)
        e
        (h b c d e (f a b c d e) (- n 1))))
  (h 1 1 1 1 1 n))
(define (work)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (solve a)) (work)))))
(work)