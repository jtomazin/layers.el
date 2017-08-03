#lang racket

(require graph)
(require "read-file.rkt")
(require "layer-macros.rkt")
;; game plan: implement predicates for the special forms that GJS uses
;;  in his type checker; make node types for each predicate; parse and
;;  analyze them

;;; FIXME:
;;  [ ] doesn't work for implicit begin (e.g., multiple exprs in a
;;  lambda)
;;  [ ] doesn't handle layers in car position (for similar reasons)

;; Considerations:
;;  - layer is kind of like define
;;  - how to manipulate environment in a define? (w/o mutation)

;; Sketch of flow analysis:
;;  - given an entry point function, identify the inputs
;;  - follow the transformation of each variable

(define pp pretty-print)

(define (layer? expr)
  (and (pair? expr)
       (eqv? (car expr) 'layer)
       (>= (length expr) 4)))

(define (get-layers exprs)
  (filter layer? exprs))
(define (remove-layers exprs)
  (filter-not layer? exprs))

;; Environment
(struct binding (name value layers) #:transparent)
(define empty-environment '())
(define (env-assoc var env)
  (cond [(empty? env) #f]
        [(equal? (binding-name (car env)) var)
         (car env)]
        [else (env-assoc var (cdr env))]))
(define add-to-env
  (case-lambda
    [(var env) (cons (binding var '? '()) env)]
    [(var val env) (cons (binding var val '()) env)]))
(define (add-layer-to-env lay env)
  (let ([old-binding (or (env-assoc (layer-name lay) env)
                         (binding (layer-name lay) '? '()))])
    (cons 
     (struct-copy binding old-binding
                  [layers (cons lay (binding-layers old-binding))])
     env)))
(define (add-layers-to-env layers env)
  (if (empty? layers)
      env
      (add-layers-to-env (cdr layers)
                         (add-layer-to-env (car layers) env))))

;; (define (associate-layers nodes layers)
;;   (map (λ (node)
;;          (add-layers
;;           (filter (λ (layer)
;;                     (eq? (node-name node)
;;                          (layer-name layer)))
;;                   layers)
;;           node))
;;        nodes))

;; (define (node-name node)
;;   (let ([expr (node-expr node)])
;;     (cond [(define-expr? expr) (define-name expr)]
;;           [(pair? expr) (car expr)]
;;           [else expr])))


;;; Nodes in the ir
;; v2 (GJS)
(struct node (expr env) #:transparent)
(define (make-node expr env)
  (node expr env))
;; (define (add-layers var lay old-node)
;;   (struct-copy node old-node [env (add-layer-to-env )]))

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
    (let ([new-env (add-layers-to-env layers env)])
      (make-node (make-application-expr
                  (annotate-expr (application-operator expr) env) ; what if layer is in car? 
                  (map (lambda (operand)
                         (annotate-expr operand new-env))
                       operands))
                 env))))

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
  (let ([new-env (add-to-env (define-name expr) env)])
    (make-node (make-define-expr (define-name expr)
                                 (annotate-expr (define-value expr)
                                                new-env))
               new-env)))

;; begin
(define (begin-expr? object)
  (and (list? object)
       (> (length object) 2)
       (eq? (car object) 'begin)))

(define begin-exprs cdr)

(define (make-begin-expr exprs)
  (list* 'begin exprs))

(define (annotate-begin expr env)
  (let loop ([exprs (begin-exprs expr)]
             [env env])
    (cond [(empty? exprs) '()]
          [(layer? (car exprs))
           (loop (cdr exprs) (add-layer-to-env (car exprs) env))]
          [else
           (let ((new-node (annotate-expr (car exprs) env)))
             (cons new-node (loop (cdr exprs) (node-env new-node))))])))

(define (annotate-expr expr env) 
  ((cond [(primitive-expr? expr) annotate-primitive]
         [(application-expr? expr) annotate-application]
         [(if-expr? expr) annotate-if]
         [(lambda-expr? expr) annotate-lambda]
         [(define-expr? expr) annotate-define]
         [(begin-expr? expr) annotate-begin]
         [else (error "expression not recognized")]) 
   expr env))

;; layers
(define (layer-type layer)
  (second layer))
(define (layer-name layer)
  (third layer))

(layer test annotate-expr
       (annotate-expr '(f a b (c 1 2) (layer type a _)) empty-environment))


;; (struct vertex (name children) #:transparent)
;; (define (node->vertex node)
;;   (if (node? node)
;;       (if (list? (node-expr node))
;;           (vertex (car (node-expr node))
;;                   (map node->vertex (cdr (node-expr node))))
;;           (vertex (node-expr node) '()))
;;       (vertex node '())))

(define (ir->graph node)
  (let ([g (directed-graph '())])
    (let loop ([expr (node-expr node)])
      (cond [(application-expr? expr)   ; (f x y) 
             (add-vertex! g )]
            [(lambda-expr? expr)        ; (lambda (a b) ...)
             ()]))))

(define fib-program
  '(define fib
     (lambda (n)
       (if (< n 2)
           n
           (+ (fib (- n 1))
              (fib (- n 2)))))))

(node->vertex (annotate-expr fib-program empty-environment))

