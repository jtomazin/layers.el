#lang racket

(require "read-file.rkt")
;; game plan: implement predicates for the special forms that GJS uses in
;;  his type checker; make node types for each predicate; parse and analyze
;;  them

;;; FIXME:
;;  [ ] doesn't work for implicit begin (e.g., multiple exprs in a lambda)
;;  [ ] doesn't handle layers in car position (for similar reasons)

(define pp pretty-print)

(define-syntax-rule (layer type symbol body)
  body)

(define (layer? expr)
  (and (pair? expr)
       (eqv? (car expr) 'layer)
       (>= (length expr) 4)))

(define (get-layers exprs)
  (filter layer? exprs))
(define (remove-layers exprs)
  (filter-not layer? exprs))

;; for now, just keep track of the defined variables
(define (add-to-env vars env)
  (append (if (list? vars)
              vars
              (list vars))
          env))

(define (associate-layers nodes layers)
  (map (λ (node)
         (add-layers
          (filter (λ (layer)
                    (eq? (node-name node)
                         (layer-name layer)))
                  layers)
          node))
       nodes))

(define (node-name node)
  (let ([expr (node-expr node)])
    (cond [(define-expr? expr) (define-name expr)]
          [(pair? expr) (car expr)]
          [else expr])))


;;; Nodes in the ir
;; v2 (GJS)
(struct node (expr layers env) #:transparent)
(define (make-node expr env)
  (node expr '() env))
(define (add-layers new-layers old-node)
  (struct-copy node old-node [layers (append new-layers
                                             (node-layers old-node))]))

;;; Forms
;; primitive
(define (primitive-expr? object)
  (or (boolean? object)
      (number? object)
      (symbol? object)))

(define (annotate-primitive expr env)
  (make-node expr env))

;; application
(define (application-expr? object)
  (and (list? object)
       (>= (length object) 1)
       (not (member (car object) '(if lambda define begin)))))

(define application-operator car) ; maybe change to (car (filter-not layer? expr))
(define application-operands cdr)

(define (make-application-expr operator operands)
  (cons operator operands))

(define (annotate-application expr env)
  (let ([layers (get-layers (application-operands expr))]
        [operands (remove-layers (application-operands expr))])
    (make-node (make-application-expr
                (annotate-expr (application-operator expr) env) ; what if layer is in car?
                (associate-layers
                 (map (lambda (operand)
                        (annotate-expr operand env))
                      operands)
                 layers))
               env)))

;; if
(define (if-expr? object)
  (and (list? object)
       (= (length object) 4)
       (eq? (car object) 'if)))

(define if-predicate cadr)
(define if-consequent caddr)
(define if-alternative cadddr)

(define (make-if-expr predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (annotate-if expr env)
  ;; how would if expressions have layers embedded?
  (make-node (make-if-expr
              (annotate-expr (if-predicate expr) env)
              (annotate-expr (if-consequent expr) env)
              (annotate-expr (if-alternative expr) env))
             env))


;; lambda
(define (lambda-expr? object)
  (and (list? object)
       (= (length object) 3)
       (equal? (car object) 'lambda)
       (bvl? (cadr object))))

(define (bvl? object)
  (and (list? object)
       (not (check-duplicates object))))

(define lambda-bvl cadr)
(define lambda-body caddr)

(define (make-lambda-expr bvl body)
  (list 'lambda bvl body))

(define (annotate-lambda expr env)
  ;; (let ([layers (get-layers (lambda-body expr))]
  ;;       [body (remove-layers (lambda-body expr))]))
  (make-node
   (make-lambda-expr (lambda-bvl expr)
                     (annotate-expr (lambda-body expr)
                                    (add-to-env (lambda-bvl expr)
                                                env)))
   env))

;; define
(define (define-expr? object)
  (and (list? object)
       (= (length object) 3)
       (eq? (car object) 'define)
       (symbol? (cadr object))))

(define define-name cadr)
(define define-value caddr)

(define (make-define-expr name value)
  (list 'define name value))

(define (annotate-define expr env)
  (make-node (make-define-expr (define-name expr)
                               (annotate-expr (define-value expr)
                                              env))
             ;; how to modify env? 
             (add-to-env (define-name expr) env))) ; fixme

;; begin
(define (begin-expr? object)
  (and (list? object)
       (> (length object) 2)
       (eq? (car object) 'begin)))

(define begin-exprs cdr)

(define (make-begin-expr exprs)
  (list* 'begin exprs))

(define (annotate-begin expr env)
  (let ([layers (get-layers (begin-exprs expr))]
        [subexprs (remove-layers (begin-exprs expr))])
    (make-node (make-begin-expr
                (map (lambda (subexpr)
                       (annotate-expr subexpr env))
                     subexprs))
               env)))

(define (annotate-expr expr env)
  ((cond [(primitive-expr? expr) annotate-primitive]
         [(application-expr? expr) annotate-application]
         [(if-expr? expr) annotate-if]
         [(lambda-expr? expr) annotate-lambda]
         [(define-expr? expr) annotate-define]
         [(begin-expr? expr) annotate-begin])
   expr env))

;; layers
(define (layer-type layer)
  (second layer))
(define (layer-name layer)
  (third layer))

(layer test annotate-expr
       (equal?
        (annotate-expr '(f a b (c 1 2) (layer type a _)) '())
        (node
         (list
          (node 'f '() '())
          (node 'a '((layer type a _)) '())
          (node 'b '() '())
          (node (list
                 (node 'c '() '())
                 (node 1 '() '())
                 (node 2 '() '()))
                '()
                '()))
         '()
         '())))

