#lang racket
(define (square x) (* x x))
(define (divisible? x y ) (= (remainder x y ) 0))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
 
(define the-empty-stream '())
 
(define (stream-null? stream)
  (null? stream))

(define (stream-ref s n)  ;取 stream里面第 n 项,n从0开始算
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
 
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) 
                   (stream-map proc (stream-cdr s)))))

(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        ((< (stream-car s1) (stream-car s2))
         (cons-stream (stream-car s1)
                      (stream-merge (stream-cdr s1) s2)))
        (else
         (cons-stream (stream-car s2) (stream-merge s1 (stream-cdr s2))))))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (weighted-pairs s1 s2 func)
  (cons-stream
   (func (list (stream-car s1) (stream-car s2)))
   (stream-merge
    (stream-map (lambda (x) (func (list x (stream-car s1)))) (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) func))))

(define (Ramanujan l)
  (define (simp a str)
    (if (= a (stream-car str))
        (simp a (stream-cdr str))
        str))
  (if (= (stream-car l) (stream-car (stream-cdr l)))
      (cons-stream (stream-car l)
                   (Ramanujan (simp (stream-car l) (stream-cdr l))))
      (Ramanujan (stream-cdr l))))
;(define (Ramanujan l) l)

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))
(define (cube x)  (* x x x))
(define weight3 (lambda (x) (+ (cube (car x)) (cube (cadr x)))))
(define lst (weighted-pairs integers integers weight3)) 
(define result-stream  (Ramanujan lst))



(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (displayln (stream-ref result-stream n)) (myloop)))))

(myloop)