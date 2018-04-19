#lang racket

(define (dui a b)
  (unique (sort (append (bu a b) (bu b a)) <)))

(define (unique a)
    (if (< (length a) 2)
        a
        (if (= (car a) (cadr a))
            (unique (cdr a))
            (cons (car a) (unique (cdr a))))))

(define (element-of? a lst)
  (if (null? lst)
      #f
      (or (= a (car lst)) (element-of? a (cdr lst)))))

(define (bu a b)
  (cond ((null? b) a)
        ((null? a) '())
        ((element-of? (car a) b) (bu (cdr a) b))
        (else (cons (car a) (bu (cdr a) b)))))
        
(define (main)
  (define a (read))
  (define b (read))
  (if (eq? a eof)
      (void)
      (begin (display (bu (unique (sort a <)) (unique (sort b <)))) (display (dui (unique (sort a < )) (unique (sort b < )))) (newline) (main))))
(main)