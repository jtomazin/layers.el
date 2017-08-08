(load "ir")
(load "graph")

;;; useful predicates...
(define any-object? (lambda (o) #t))
(define (list-of-symbols? o) (and (list? o)
                                  (every symbol? o)))

;;; Environment
(define environment? alist?)
(define empty-environment '())
(define empty-env empty-environment)
(define extend-env (make-generic-operator 3 'extend-env))
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
(define make-graph (make-generic-operator 2 'make-graph))

(define (eval-subexprs subexprs env)
  (map (lambda (expr) (make-graph expr env)) subexprs))

;; literals
(defhandler make-graph
  (lambda (expr env)
    (add-vertex (lit-datum expr) empty-graph))
  lit? environment?)

;; variables
(defhandler make-graph
  (lambda (expr env)
    (add-vertex
     ;; (string->symbol
     ;;  (string-append
     ;;   (symbol->string (var-name expr))
     ;;   "="
     ;;   (apply-env (var-name expr) env)))
     (var-name expr)
     empty-graph))
  var? environment?)

;; application
(defhandler make-graph
  (lambda (expr env)
    (let ((proc (make-graph (app-rator expr) env))
          (args (map (lambda (arg)
                       (make-graph arg env))
                     (app-rands expr))))
      (fold-left (lambda (arg)
                   (add-edge (vertex-name arg) (vertex-name proc)))
                 empty-graph
                 args)
      ;; (apply-proc proc args)
      ))
  app? environment?)

;; lambda
(defhandler make-graph
  (lambda (expr env)
    (make-closure (lambda-formals expr)
                  (lambda-body expr)
                  env))
  lambda? environment?)

;; if
(defhandler make-graph
  (lambda (expr env)
    (if (make-graph (if-pred expr) env)
        (make-graph (if-consq expr) env)
        (make-graph (if-alt expr) env)))
  if? environment?)

;; define
(defhandler make-graph
  (lambda (expr env)
    (error "define unsupported for now"))
  define? environment?)

;; let
(defhandler make-graph
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

(make-graph (parse '((lambda (f1)
                       (f1 f1 9))
                     (lambda (f n)
                       (if (< n 2)
                           n
                           (+ (f f (- n 2))
                              (f f (- n 1)))))))
            base-env)

(make-graph (parse '((lambda (x) x) 4)) base-env)

(make-graph (parse '((lambda (l)
                       (let ((x (car l))
                             (y (car (cdr l))))
                         (+ x y)
                         (- x y)))
                     (list 3 4)))
            base-env)
