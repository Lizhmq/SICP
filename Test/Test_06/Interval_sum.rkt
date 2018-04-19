#lang racket

(define n (read))

(define a '())
(define (max x y)
  (if (> x y)
      x
      y))
(define (min x y)
  (if (< x y)
      x
      y))
(define (solve-1 l r llst)
  (let ((l2 (car llst))
        (r2 (cadr llst))
        (num (caddr llst)))
    (if (and (>= r l2) (<= l r2))
        (* num (+ (- (min r r2) (max l l2)) 1))
        0)))

(define (solve l r lst)
  (if (null? lst)
      0
      (+ (solve l r (cdr lst)) (solve-1 l r (car lst)))))

(define (main)
  (if (= n 0)
      (void)
      (begin
        (let ((b (read)))
          (if (= b 1)
              (set! a (cons (list (read) (read) (read)) a))
              (displayln (solve (read) (read) a))))
        (set! n (- n 1))
        (main))))
(main)