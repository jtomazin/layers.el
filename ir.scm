(load "ghelper")
(load "properties")

;;; Parse is a generic operator, with a handler for each form
(define parse (make-generic-operator 1 'parse))
;; parse: list -> record; unparse: record -> list
(define unparse (make-generic-operator 1 'unparse))

;; props is used to access the props table of every expression
(define props (make-generic-operator 1 'props))

(define (parse-subexprs subexprs)
  (map parse subexprs))

;;; Forms
(define special-forms
  ;; todo: add records/structs later
  '(lambda if define begin do let quote set!))

;; literals
(define-record-type <lit>
  (%make-lit datum props)
  lit?
  (datum lit-datum)
  (props lit-props))
(define (make-lit datum) (%make-lit datum (make-props-table)))
(define (lit-expr? expr) (or (boolean? expr)
                             (number? expr)
                             (string? expr)
                             (char? expr)))
(define (parse-lit expr)
  (make-lit expr))
(defhandler parse parse-lit lit-expr?)
(defhandler unparse (lambda (expr) (lit-datum expr)) lit?)
(defhandler props lit-props lit?)

;; variables
(define-record-type <var>
  (%make-var name props)
  var?
  (name var-name)
  (props var-props))
(define (make-var name) (%make-var name (make-props-table)))
(define var-expr? symbol?)
(define (parse-var expr)
  (make-var expr))
(defhandler parse parse-var var-expr?)
(defhandler unparse (lambda (expr) (var-name expr)) var?)
(defhandler props var-props var?)

;; (operator rand1 rand2 ...)
(define-record-type <app>
  (%make-app rator rands props)
  app?
  (rator app-rator)
  (rands app-rands)
  (props app-props))
(define (make-app rator rands) (%make-app rator rands (make-props-table)))
(define (app-expr? expr) (and (pair? expr)
                              (not (member (car expr) special-forms))))
(define (parse-app expr)
  (make-app (parse (car expr)) (parse-subexprs (cdr expr))))
(defhandler parse parse-app app-expr?)
(defhandler unparse
  (lambda (expr)
    `(,(unparse (app-rator expr)) ,@(map unparse (app-rands expr))))
  app?)
(defhandler props app-props app?)

;; (lambda (formals) body)
(define-record-type <lambda>
  (%make-lambda formals body props)
  lambda?
  (formals lambda-formals)
  (body lambda-body)
  (props lambda-props))
(define (make-lambda formals body) (%make-lambda formals body (make-props-table)))
(define (lambda-expr? expr) (and (pair? expr) (eq? (car expr) 'lambda)))
(define (parse-lambda expr)
  (let ((formals (second expr))
        (body (cddr expr)))
    (make-lambda formals
                 (parse-subexprs body))))
(defhandler parse parse-lambda lambda-expr?)
(defhandler unparse
  (lambda (expr)
    `(lambda ,(lambda-formals expr) ,@(map unparse (lambda-body expr))))
  lambda?)
(defhandler props lambda-props lambda?)

;; (if pred consq alt)
(define-record-type <if>
  (%make-if pred consq alt props)
  if?
  (pred if-pred)
  (consq if-consq)
  (alt if-alt)
  (props if-props))
(define (make-if pred consq alt) (%make-if pred consq alt (make-props-table)))
(define (if-expr? expr) (and (pair? expr) (eq? (car expr) 'if)))
(define (parse-if expr)
  (make-if (parse (second expr))
           (parse (third expr))
           (parse (fourth expr))))
(defhandler parse parse-if if-expr?)
(defhandler unparse
  (lambda (expr)
    (list 'if (unparse (if-pred expr))
          (unparse (if-consq expr))
          (unparse (if-alt expr))))
  if?)
(defhandler props if-props if?)

;; (define var expr)
(define-record-type <define>
  (%make-define var expr props)
  define?
  (var define-var)
  (expr define-expr)
  (props define-props))
(define (make-define var expr) (%make-define var expr (make-props-table)))
(define (define-expr? expr) (and (pair? expr) (eq? (car expr) 'define)))
(define (parse-define expr)
  (make-define (second expr)
               (parse (third expr))))
(defhandler parse parse-define define-expr?)
(defhandler unparse
  (lambda (expr)
    (list 'define (define-var expr) (unparse (define-expr expr))))
  define?)
(defhandler props define-props define?)

;; let
(define-record-type <let>
  (%make-let decls body props)
  let?
  (decls let-decls)
  (body let-body)
  (props let-props))
(define (make-let decls body) (%make-let decls body (make-props-table)))
(define (let-expr? expr) (and (pair? expr) (eq? (car expr) 'let)))
(define (parse-let expr)
  (make-let (map (lambda (dec)
                   (parse-decl-for-let dec))
                 (second expr))
            (parse-subexprs (cddr expr))))
(defhandler parse parse-let let-expr?)
(defhandler unparse
  (lambda (expr)
    (list 'let (map unparse-decl-for-let (let-decls expr))
          (unparse (let-body expr))))
  let?)
(defhandler props let-props let?)

(define-record-type <decl>
  (make-decl var expr)
  decl?
  (var decl-var)
  (expr decl-expr))
(define (parse-decl-for-let expr)
  (make-decl (first expr) (parse (second expr))))
(define (unparse-decl-for-let decl)
  (list (decl-var expr) (unparse (decl-expr decl))))

;; TODO: top level vs internal definitions?
;;  probably recognize top level defines and convert local defines to letrec
