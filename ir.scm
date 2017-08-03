(load "ghelper")

;;; Parse is a generic operator, with a handler for each form
(define parse (make-generic-operator 2 'parse))

(define (parse-subexprs subexprs env)
  (map (lambda (exp) (parse exp env)) subexprs))

;;; useful predicates...
(define any-object? (lambda (o) #t))
(define (list-of-symbols? o) (and (list? o)
                                  (every symbol? o)))

;;; Environment
(define environment? alist?)
(define empty-environment '())
(define extend-env (make-generic-operator 3 'extend-env))

(defhandler extend-env
  (lambda (vars vals env)
    (unless (= (count vars) (count vals))
            (error "Number of variables and values must match"))
    (append (map cons vars vals) env))
  list-of-symbols? list? environment?)
(defhandler extend-env
  (lambda (var val env)
    (extend-env (list var) (list val) env))
  symbol? any-object? environment?)

;;; Forms
(define special-forms
  ;; todo: add records/structs later
  '(lambda if define begin do let quote set!))

;; variables
(define-record <var>
  (make-var name)
  var?
  (name var-name))
(define variable-expr? symbol?)
(define (parse-var expr env)
  (make-var expr))
(defhandler parse parse-var var? environment?)

;; literals
(define-record-type <lit>
  (make-lit datum)
  lit?
  (datum lit-datum))
(define (lit-expr? expr) (or (boolean? expr)
                             (number? expr)
                             ;; (string? expr)
                             ;; (char? expr)
                             ))
(define (parse-lit expr env)
  (make-lit expr))
(defhandler parse parse-lit lit-expr? environment?)

;; (operator rand1 rand2 ...)
(define-record-type <app>
  (make-app rator rands)
  app?
  (rator app-rator)
  (rands app-rands))
(define (app-expr? expr) (and (pair? expr)
                              (not (member (car expr) special-forms))))
(define (parse-app expr env)
  (make-app (car expr) (parse-subexprs (cdr expr) env)))
(defhandler parse parse-app app-expr? environment?)

;; (lambda (formals) body)
(define-record-type <lambda>
  (make-lambda formals body env)
  lambda?
  (formals lambda-formals)
  (body lambda-body)
  (env lambda-env))
(define (lambda-expr? expr) (eq? (car expr) 'lambda))
(define (parse-lambda expr env)
  (let ((formals (second expr))
        (body (cddr expr)))
    (make-lambda formals
                 env ;; ???
                 body)))
(defhandler parse parse-lambda lambda-expr? environment?)

;; (if pred consq alt)
(define-record-type <if>
  (make-if pred consq alt)
  if?
  (pred if-pred)
  (consq if-consq)
  (alt if-alt))
(define (if-expr? expr) (eq? (car expr) 'if))
(define (parse-if expr env)
  (make-if (parse (second expr) env)
           (parse (third expr) env)
           (parse (fourth expr) env)))
(defhandler parse parse-if if-expr? environment?)

;; (define var expr)
(define-record-type <define>
  (make-define var expr)
  define?
  (var define-var)
  (expr define-expr))
(define define-expr? (eq? (car expr) 'define))
(define (parse-define expr env)
  (make-define (second expr)
               (parse (third expr) env)))
(defhandler parse parse-define define-expr? environment?)

;; let
(define-record-type <let>
  (make-let decls body)
  let?
  (decls let-decls)
  (body let-body))
(define let-expr? (eq? (car expr) 'let))
(define (parse-let expr env)
  (make-let (map (lambda (dec)
                   (parse-decl-for-let dec env))
                 (second expr))
            (parse-subexprs (cddr expr) env)))
(defhandler parse parse-let let-expr? environment?)

(define-record <decl>
  (make-decl var expr)
  decl?
  (var decl-var)
  (expr decl-expr))
(define (parse-decl-for-let expr env)
  (make-decl (first expr) (parse (second expr) env)))

;; top level vs internal definitions?

(define (eval-expr expr env)
  )
