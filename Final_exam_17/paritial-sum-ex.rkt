#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
(define (partial-sums-ex op s)
  (define (stream-map func s)
    (if (stream-null? s) the-empty-stream
        (cons-stream (func (stream-car s)) (stream-map func (stream-cdr s)))))
  (define (trans-stream s)
    (cons-stream (list (stream-car s))
                 (stream-map (lambda (x) (cons (stream-car s) x))
                      (trans-stream (stream-cdr (stream-cdr s))))))
  (define (inter s1 s2)
    (if (stream-null? s1) the-empty-stream
        (cons-stream (func (stream-car s1) op) (inter s2 (stream-cdr s1)))))
  (define (func lst op)
    (if (= (length lst) 1)
        (car lst)
        (apply op lst)))
  (let ((s1 (trans-stream s))
        (s2 (trans-stream (stream-cdr s))))
    (inter s1 s2)))
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