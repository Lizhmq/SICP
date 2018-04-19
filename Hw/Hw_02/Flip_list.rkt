#lang racket

(define (flip a)
  (if (eq? a '())
      '()
      (let ((b (car a)))
      (append (flip (cdr a)) (cons b '())))))

(define (work1)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (flip a)) (work1)))))

(work1)