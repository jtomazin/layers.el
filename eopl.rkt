#lang racket

;; (struct lit-expr (datum))
;; (struct varref-expr (var))
;; (struct lambda-expr (formal body))
;; (struct app- (rator rand))

;;   (lambda (datum)
;;     (cond
;;       ((number? datum) (lit datum))
;;       ((symbol? datum) (varref datum))
;;       ((pair? datum)
;;        (if (eq? (car datum) 'lambda)
;;            (make-lambda (caadr datum) (parse (caddr datum)))
;;            (make-app (parse ))))
;;       )))

(define special-forms
  ;; todo: add records/structs later
  '(lambda if define begin do let quote set!))

;; variables
(struct var (name)
  #:constructor-name make-var #:transparent)
(define variable-expr? symbol?)

;; literals
(struct lit (datum)
  #:constructor-name make-lit #:transparent)
(define (lit-expr? expr) (or (boolean? expr)
                             (number? expr)
                             ;; (string? expr)
                             ;; (char? expr)
                             ))

;; (operator rand1 rand2 ...)
(struct app (rator rands)
  #:constructor-name make-app #:transparent)
(define (app-expr? expr) (and (pair? expr)
                              (not (member (car expr) special-forms))))

;; (lambda (formals) body)
(struct lambda (formals body env)
  #:constructor-name make-lambda #:transparent)
(define lambda? lambda-node?)
(define (lambda-expr? expr) (eq? (car expr) 'lambda))

;; (if pred consq alt)
(struct if-node (pred consq alt)
  #:constructor-name make-if #:transparent)
(define if? if-node?)
(define (if-expr? expr) (eq? (car expr) 'if))

;; (define var expr)
(struct define-node (var expr)
  #:constructor-name make-define #:transparent)
(define define? define-node?)
()
