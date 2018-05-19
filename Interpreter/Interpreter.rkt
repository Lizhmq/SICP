#lang racket

;require r5rs用于实现mcons和set!
(require r5rs)


;my-eval定义
(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)                            ;自求值表达式
        ((variable? exp) (lookup-variable-value exp env))       ;变量
        ((and? exp) (eval-and (operands exp) env))
        ((or? exp) (eval-or (operands exp) env))                
        ((let? exp) (eval-let exp env))                   ;let表达式
        ((quoted? exp) (text-of-quotation exp))                 ;单引号表达式（符号）
        ((assignment? exp) (eval-assignment exp env))           ;赋值set!
        ((definition? exp) (eval-definition exp env))           ;定义define
        ((if? exp) (eval-if exp env))                           ;if表达式
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))              ;条件表达式
        ((application? exp)                                     ;上述都不成立时，视为过程调用
         (my-apply (my-eval (operator exp) env) (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))




;表达式类型判断，表达式部分分割
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)                ;单引号开头的表达式会被转换成(quote ...)的list
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)            ;(set! x y)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)            ;define有两种形式 (define <var> <value>)
  (tagged-list? exp 'define))                        ;(define (<var> paras) (body))
(define (definition-variable exp)            ;后者等价于(define <var> (lambda (paras) (body)))
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)           ;paras
                   (cddr exp))))         ;body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)                 ;alternative部分可能不存在
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond->if exp)                      
  (expand-clauses (cond-clauses exp)))          
(define (special-cond? clause)               ;特殊形式cond ([test] => [recipient])的实现
  (and (pair? (cdr clause)) (eq? (cadr clause) '=>)))
(define (expand-clauses clauses)
  (if (null? clauses)
      (void)
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (special-cond? first)
                         (list (caddr first) (car first))
                         (sequence->exp
                          (if (null? (cond-actions first))
                              (list (cond-predicate first))        ;cond-actions部分不存在时返回cond-predicate部分
                              (cond-actions first))))              ;以actions的形式返回，故加上list
                     (expand-clauses rest))))))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (list-of-values lst env)
  (map (lambda (x) (my-eval x env)) lst))

(define (let? exp) (tagged-list? exp 'let))
(define (let-clause exp) (cadr exp))
(define (let-body exp) (cddr exp))





;表达式求值
(define (eval-and ops env)          ;and表达式，如果ops都为真就返回最后一个值
  (if (null? ops)
      (error "eval-and error")
      (if (null? (cdr ops))
          (my-eval (car ops) env)
          (if (my-eval (car ops) env)
              (eval-and (cdr ops) env)
              false))))
(define (eval-or ops env)           ;or表达式，返回第一个不false的值
  (if (null? ops)
      false
      (let ((cur (my-eval (car ops) env)))
        (if cur
            cur
            (eval-or (cdr ops) env)))))

(define (eval-let exp env)                  ;转换为lambda表达式，参数递归my-eval求值
  (let* ((vals (map (lambda (x) (my-eval x env)) (map cadr (let-clause exp))))
         (func (make-lambda
                (map (lambda (x) (car x)) (let-clause exp))   ;paras
                (let-body exp))))                             ;body
    (my-apply (my-eval func env) vals)))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env))

(define (true? x)
  (not (eq? false x)))
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env) (eval-sequence (rest-exps exps) env))))

(define (lookup-variable-value var env)
  (define (env-loop env)              ;在env中寻找var的约束
    (define (scan vars vals)
      (cond ((null? vars)             ;env的vars列表中没有找到var
             (env-loop (enclosing-environment env)))   ;去外围环境找
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)           ;最外层环境
        (error "Unbound variable" var)            ;未约束变量
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))





;环境
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
;初始过程，用于构建初始环境
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'list list)
        (list 'cons cons)
        (list 'pair? pair?)
        (list 'append append)
        (list 'caddr caddr)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'assoc assoc)
        (list 'length length)
        (list 'not not)
        (list 'eq? eq?)
        (list 'remainder remainder)
        (list 'sqrt sqrt)
        (list 'symbol? symbol?)
        (list 'number? number?)
        (list 'null? null?)))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))     ;初始过程用'primitive标签
       primitive-procedures))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define glb-env (setup-environment))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (my-apply procedure arguments)                 ;过程apply时在新的内层环境中求值
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))




;输入输出
(define input-prompt ">>> ")
(define output-prompt "")
(define (driver-loop)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (begin
          (let ((output (my-eval input glb-env)))
            (if (not (eq? output (void)))
                (user-print output)))
          (driver-loop)))))
(define (prompt-for-input string)
  (display string))
(define (announce-output string)
  (display string))
(define (user-print object)
  (if (compound-procedure? object)
      (displayln "#<procedure>")
      (displayln object)))
;将大括号转换为小括号
(print-mpair-curly-braces #f)
(driver-loop)