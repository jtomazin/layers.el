(load "ir")

;;; useful predicates...
(define any-object? (lambda (o) #t))
(define (list-of-symbols? o) (and (list? o)
                                  (every symbol? o)))

;;; Environment
(define environment? alist?)
(define empty-environment '())
(define empty-env empty-environment)
(define extend-env
  (make-generic-operator 3 'extend-env
                         (lambda (a b c)
                           (error "extend-env: No handler for args" a b c))))
(defhandler extend-env
  (lambda (vars vals env)
    (if (not (= (length vars) (length vals)))
        (error "Number of variables and values must match"))
    (append (map cons vars vals) env))
  list-of-symbols? list? environment?)
(defhandler extend-env
  (lambda (var val env)
    (extend-env (list var) (list val) env))
  symbol? any-object? environment?)

(define primitive-procs
  '(+ - / * modulo remainder
      car cdr cons list
      eq? eqv? equal?
      < > <= >= =))

(define base-env
  (extend-env primitive-procs
              primitive-procs
              empty-env))

(define (apply-env var env)
  (let ((binding (assoc var env)))
    (if binding
        (cdr binding)
        (error "reference to unbound variable" var))))

;;; Closures
(define-record-type <closure>
  (make-closure formals body env)
  closure?
  (formals closure-formals)
  (body closure-body)
  (env closure-env))

;;; Evaluation 
(define eval-ast
  (make-generic-operator 2 'eval-ast
                         (lambda (a b)
                           (error "eval-ast: No handler for" a b))))

(define (eval-subexprs subexprs env)
  (map (lambda (expr) (eval-ast expr env)) subexprs))

;; literals
(defhandler eval-ast
  (lambda (expr env)
    (lit-datum expr))
  lit? environment?)

;; variables
(defhandler eval-ast
  (lambda (expr env)
    (apply-env (var-name expr) env))
  var? environment?)

;; application
(defhandler eval-ast
  (lambda (expr env)
    (let ((proc (eval-ast (app-rator expr) env))
          (args (map (lambda (arg)
                       (eval-ast arg env))
                     (app-rands expr))))
      (apply-proc proc args)))
  app? environment?)

;; lambda
(defhandler eval-ast
  (lambda (expr env)
    (make-closure (lambda-formals expr)
                  (lambda-body expr)
                  env))
  lambda? environment?)

;; if
(defhandler eval-ast
  (lambda (expr env)
    (if (eval-ast (if-pred expr) env)
        (eval-ast (if-consq expr) env)
        (eval-ast (if-alt expr) env)))
  if? environment?)

;; define
(defhandler eval-ast
  (lambda (expr env)
    (error "define unsupported for now"))
  define? environment?)

;; let
(defhandler eval-ast
  (lambda (expr env)
    (let ((vars (map decl-var (let-decls expr)))
          (exps (map decl-expr (let-decls expr))))
      (last (eval-subexprs (let-body expr)
                           (extend-env vars (eval-subexprs exps env)
                                       env)))))
  let? environment?)

;;; Apply
(define apply-proc
  (make-generic-operator 2 'eval-proc
                         (lambda (a b)
                           (error "apply-proc: No handler for" a b))))

(define (primitive-proc? op)
  (member op primitive-procs))

(defhandler apply-proc
  (lambda (proc args)
    (apply (eval proc system-global-environment) args))
  primitive-proc? list?)

(defhandler apply-proc
  (lambda (proc args)
    (let ((new-env (extend-env (closure-formals proc)
                               args
                               (closure-env proc))))
      (last (eval-subexprs (closure-body proc) new-env))))
  closure? list?)

;; tests

(eval-ast (parse '((lambda (f1)
                     (f1 f1 9))
                   (lambda (f n)
                     (if (< n 2)
                         n
                         (+ (f f (- n 2))
                            (f f (- n 1)))))))
          base-env)

(eval-ast (parse '((lambda (x) x) 4)) base-env)

(eval-ast (parse '((lambda (l)
                     (let ((x (car l))
                           (y (car (cdr l))))
                       (+ x y)
                       (- x y)))
                   (list 3 4)))
          base-env)
