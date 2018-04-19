#lang racket

(define (merge a b)
  (if (eq? a '())
      (cons b '())
      (cons (car a) (merge (cdr a) b))))

(define (pp a)
  (if (and (pair? a) (pair? (cdr a)))
      (cons (+ (car a) (cadr a)) (pp (cdr a)))
      '()))

(define (cal a)
  (cond ((= a 0) (void))
        ((= a 1) (begin (displayln "1 ") (cons 1 '())))
        ((= a 2) (begin (displayln "1 ") (displayln "1 1 ") (cons 1 (cons 1 '()))))
        (else
         (let* ((lst (cal (- a 1)))
               (b (cons 1 (merge (pp lst) 1))))
               (begin (show b) (newline) b)))))
       
(define (show a)
  (if (eq? a '())
      (void)
      (begin (display (car a)) (display #\ ) (show (cdr a)))))

(define (solve)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (cal a) (solve)))))

(solve)