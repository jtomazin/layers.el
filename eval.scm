(load "ir")

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

(define eval-ast (make-generic-operator 2 'eval-ast))

;; literals
(defhandler eval-ast
  (lambda (expr env)
    )
  lit? environment?)

;; variables
(defhandler eval-ast
  (lambda (expr env)
    )
  var? environment?)

;; application
(defhandler eval-ast
  (lambda (expr env)
    )
  app? environment?)

;; lambda
(defhandler eval-ast
  (lambda (expr env)
    )
  lambda? environment?)

;; if
(defhandler eval-ast
  (lambda (expr env)
    )
  if? environment?)

;; define
(defhandler eval-ast
  (lambda (expr env)
    )
  define? environment?)

;; let
(defhandler eval-ast
  (lambda (expr env)
    )
  let? environment?)
