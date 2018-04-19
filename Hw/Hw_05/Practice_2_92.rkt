#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现,不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (install-polynomial-package)
  (define (switch x)
    (cond ((eq? x 'a) 5)
          ((eq? x 'b) 4)
          ((eq? x 'c) 3)
          ((eq? x 'd) 2)
          ((eq? x 'e) 1)
          (else (error "char?"))))
  (define (add-terms L1 L2)
    (cond ((null? L1) L2)
          ((null? L2) L1)
          (else
           (let ((t1 (car L1)) (t2 (car L2)))
             (cond ((> (car t1) (car t2))
                    (adjoin-term
                     t1 (add-terms (cdr L1) L2)))
                   ((< (car t1) (car t2))
                    (cons
                     t2 (add-terms L1 (cdr L2))))
                   (else
                    (cons
                     (list (car t1)
                           (add (cadr t1) (cadr t2)))
                     (add-terms (cdr L1) (cdr L2)))))))))
  (define (adjoin-term a lst)
    (cons a lst))
  (define (tag a)
    (cons 'polynomial a))
  ;(define (ap lst a)
   ; (if (null? lst)
    ;    (list a)
     ;   (let ((num1 (caar lst)) (num2 (car a)))
      ;    (cond ((< num1 num2) (cons a lst))
       ;         ((= num1 num2) (cons (cons (num1 (add (cadr lst) (cdr a)))) (cdr lst)))
        ;        (else
         ;        (cons (car lst) (ap (cdr lst) a)))))))
  (put 'make 'polynomial
       (lambda (variable term-list)
         (tag (cons variable term-list))))
  (put 'make 'polynomial-term
       (lambda (order coeff)
         (list order coeff)))
  (define (up a b)
    (list (list 0 (tag b))))
  (define (add0 a b)
    (let ((t1 (switch (car a))) (t2 (switch (car b))))
      (cond ((= t1 t2)
             (tag (cons (car a) (add-terms (cdr a) (cdr b)))))
            ((> t1 t2)
             (tag (cons (car a) (add-terms (cdr a) (up (car a) b)))))
            (else
             (add0 b a)))))
  (define (mul-terms L1 L2)
    (if (null? L1)
        '()
        (add-terms (mul-term-by-all (car L1) L2)
                   (mul-terms (cdr L1) L2))))
  (define (mul-term-by-all t1 L)
    (if (null? L)
        '()
        (let ((t2 (car L)))
          (cons
           (list (+ (car t1) (car t2))
                 (mul (cadr t1) (cadr t2)))
           (mul-term-by-all t1 (cdr L))))))
  (define (add1 p i)
    (tag (cons (car p) (add-terms (cdr p) (list (list 0 (cons 'integer i)))))))
  (define (add2 i p)
    (add1 p i))
  (define (mul0 a b)
    (let ((t1 (switch (car a))) (t2 (switch (car b))))
      (cond ((= t1 t2)
             (tag (cons (car a) (mul-terms (cdr a) (cdr b)))))
            ((> t1 t2)
             (tag (cons (car a) (mul-terms (cdr a) (up (car a) b)))))
            ((< t1 t2)
             (mul0 b a)))))
  (define (mul1 p i)
    (tag (cons (car p) (mul-terms (cdr p) (list (list 0 (cons 'integer i)))))))
  (define (mul2 a b) (mul1 b a))
  (put 'add '(polynomial integer) add1)
  (put 'add '(integer polynomial) add2)
  (put 'add '(polynomial polynomial) add0)
  (put 'mul '(polynomial polynomial) mul0)
  (put 'mul '(polynomial integer) mul1)
  (put 'mul '(integer polynomial) mul2)
  (void))
(define (simplex-one a)
  (if (eq? (caadr a) 'integer)
      (list (car a) (cdadr a))
      (list (car a) (simplex-list (cadr a)))))
(define (simplex-list a)
  (define (rout lst)
    (if (null? lst)
        '()
        (cons (simplex-one (car lst)) (rout (cdr lst)))))
  (cons (cadr a) (rout (cddr a))))
(define (display-poly a)
  (displayln (simplex-list a)))
(define (build-one a)
  (if (number? (cadr a))
      (list (car a) (cons 'integer (cadr a)))
      (list (car a) (build-poly (cadr a)))))
(define (build-poly a)
  (define (rout lst)
    (if (null? lst)
        '()
        (cons (build-one (car lst)) (rout (cdr lst)))))
  (cons 'polynomial (cons (car a) (rout (cdr a)))))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "????")))))
(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)