(load "ghelper")

;;; Parse is a generic operator, with a handler for each form
(define parse (make-generic-operator 1 'parse (lambda (a)
                                                (error "No handler for" a))))

(define (parse-subexprs subexprs)
  (map parse subexprs))

;;; Forms
(define special-forms
  ;; todo: add records/structs later
  '(lambda if define begin do let quote set!))

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
(define (parse-lit expr)
  (make-lit expr))
(defhandler parse parse-lit lit-expr?)

;; variables
(define-record-type <var>
  (make-var name)
  var?
  (name var-name))
(define var-expr? symbol?)
(define (parse-var expr)
  (make-var expr))
(defhandler parse parse-var var-expr?)

;; (operator rand1 rand2 ...)
(define-record-type <app>
  (make-app rator rands)
  app?
  (rator app-rator)
  (rands app-rands))
(define (app-expr? expr) (and (pair? expr)
                              (not (member (car expr) special-forms))))
(define (parse-app expr)
  (make-app (parse (car expr)) (parse-subexprs (cdr expr))))
(defhandler parse parse-app app-expr?)

;; (lambda (formals) body)
(define-record-type <lambda>
  (make-lambda formals body)
  lambda?
  (formals lambda-formals)
  (body lambda-body))
(define (lambda-expr? expr) (and (pair? expr) (eq? (car expr) 'lambda)))
(define (parse-lambda expr)
  (let ((formals (second expr))
        (body (cddr expr)))
    (make-lambda formals
                 (parse-subexprs body))))
(defhandler parse parse-lambda lambda-expr?)

;; (if pred consq alt)
(define-record-type <if>
  (make-if pred consq alt)
  if?
  (pred if-pred)
  (consq if-consq)
  (alt if-alt))
(define (if-expr? expr) (and (pair? expr) (eq? (car expr) 'if)))
(define (parse-if expr)
  (make-if (parse (second expr))
           (parse (third expr))
           (parse (fourth expr))))
(defhandler parse parse-if if-expr?)

;; (define var expr)
(define-record-type <define>
  (make-define var expr)
  define?
  (var define-var)
  (expr define-expr))
(define (define-expr? expr) (and (pair? expr) (eq? (car expr) 'define)))
(define (parse-define expr)
  (make-define (second expr)
               (parse (third expr))))
(defhandler parse parse-define define-expr?)

;; let
(define-record-type <let>
  (make-let decls body)
  let?
  (decls let-decls)
  (body let-body))
(define (let-expr? expr) (and (pair? expr) (eq? (car expr) 'let)))
(define (parse-let expr)
  (make-let (map (lambda (dec)
                   (parse-decl-for-let dec))
                 (second expr))
            (parse-subexprs (cddr expr))
            ))
(defhandler parse parse-let let-expr?)

(define-record-type <decl>
  (make-decl var expr)
  decl?
  (var decl-var)
  (expr decl-expr))
(define (parse-decl-for-let expr)
  (make-decl (first expr) (parse (second expr))))

;; top level vs internal definitions?
