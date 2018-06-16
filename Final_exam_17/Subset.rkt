#lang racket

(define (cmp a b)
  (cond ((null? a) true)
        ((null? b) false)
        ((< (car a) (car b)) true)
        ((> (car a) (car b)) false)
        (else (cmp (cdr a) (cdr b)))))

(define (subset lst)
  (if (null? lst)
      (list '())
      (append (subset (cdr lst))
              (map (lambda (x) (cons (car lst) x)) (subset (cdr lst))))))

(define (loop)
  (define a (read))
  (cond ((eq? a eof) (void))
        (else (displayln (sort (subset (sort a <)) cmp))
              (loop))))
(loop)