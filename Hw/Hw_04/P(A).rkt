#lang racket

(define (compare lsta lstb)
  (cond ((null? lsta) #f)
        ((null? lstb) #t)
        ((not (list? lsta)) (< lsta lstb))
        ((compare (car lsta) (car lstb)) (compare (cdr lsta) (cdr lstb)))
        ((compare (car lsta) (car lstb)) #t)
        (else #f)))

(define (unique a)
    (if (< (length a) 2)
        a
        (if (= (car a) (cadr a))
            (unique (cdr a))
            (cons (car a) (unique (cdr a))))))

(define (mi lst)
  (if (null? lst)
      (list '())
      (let ((smi (mi (cdr lst))))
        (append (map (lambda (x) (cons (car lst) x)) smi) smi))))
        
(define (P n lst)
  (if (= n 1)
      (mi lst)
      (sort (P (- n 1) (mi lst)) compare)))

(define (main)
  (define a (read))
  (define b (read))
  (if (eq? a eof)
      (void)
      (begin (displayln (P b (unique (sort a <)))) (main))))
(main)