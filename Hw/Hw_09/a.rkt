#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))

;self-evaluate
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;variable
(define (variable? exp) (symbol? exp))

;quote
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      (false)))
(define (text-of-quotation exp) (cadr exp))

;set!
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(deifne (assignment-value exp) (caddr exp))

;function definition
(define (definiton? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)               ;(define <var> <value>)
      (caadr exp)))            ;(define (<var> paras) <body>)
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))   ;body can be a list

;lambda expression
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))       ;body can be alist
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

