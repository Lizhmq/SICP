#lang racket
(define (div? a b)
  (= (remainder a b) 0))

(define (cc a)
  (define (solve x res)
     (if (div? x a)
          (solve (/ x a) (+ res 1))
          res))
  (lambda (x) (solve x 0)))

(define car (cc 2))
(define cdr (cc 3))
(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)