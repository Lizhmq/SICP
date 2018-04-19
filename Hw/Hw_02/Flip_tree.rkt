#lang racket

(define (tree t)
  (cond ((eq? t '()) '())
        ((list? t) (append (tree (cdr t)) (cons (tree (car t)) '())))
        (else t)))

(define (work)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (tree a)) (work)))))

(work)