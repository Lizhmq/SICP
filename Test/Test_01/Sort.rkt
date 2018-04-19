#lang racket

(define (merge a b)
  (cond ((pair? a)
         (let ((fir (car a)))
           (cond ((< b fir) (cons b a))
                 ((= b fir) a)
                 ((cons fir (merge (cdr a) b))))))
        ((eq? a '()) (cons b '()))
        ((= a b) (cons a '()))
        ((> a b) (cons b (cons a '())))
        (else (cons a (cons b '())))))
        
(define (show a)
  (if (eq? a '())
      (void)
      (begin (display (car a)) (display #\ ) (show (cdr a)))))

(define (solve a)
  (let ((b (read)))
    (if (eq? b eof)
        a
        (solve (merge a b)))))

(show (solve '()))