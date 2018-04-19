#lang racket
(define (cont-frac-iter N D k)
  (define (solve n)
    (if (= n k)
        (/ (N k) (D k))
        (/ (N n) (+ (D n) (solve (+ n 1))))))
  (solve 1))
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)