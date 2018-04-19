#lang racket

(define (flat a)
  (cond ((eq? a '()) '())
        ((list? a) (append (flat (car a)) (flat (cdr a))))
        (else (cons a '()))))

(define (work2)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (flat a)) (work2)))))

(work2)