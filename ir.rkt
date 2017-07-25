#lang racket

;; game plan: implement predicates for the special forms that GJS uses in
;;  his type checker; make node types for each predicate; parse and analyze
;;  them
;;; Specifics:
;;   - implement nodes as structs
;;   - add env to structs
;;   - add special forms (if, lambda, define, etc)

(define pp pretty-print)

(define-syntax-rule (layer type symbol body)
  body)

(define (layer? expr)
  (and (pair? expr)
       (eqv? (car expr) 'layer)
       (>= (length expr) 4)))

(define (annotate nodes layers)
  (map (λ (node)
         (add-layers
          (filter (λ (layer)
                    (eq? (node-name node)
                         (layer-name layer)))
                  layers)
          node))
       nodes))

(define (process sexpr)
  ;; make a node for each child
  ;; annotate each child with any layers at this level
  ;; recurse on children of children if any 
  (if (pair? sexpr)
      (let* ([children (filter-not layer? (cdr sexpr))]
             [nodes (map process children)] ; recurse
             [layers (filter layer? (cdr sexpr))])
        (make-node sexpr
                   (annotate nodes layers)))
      (make-node sexpr '())))

;; nodes in the ir
(struct node (type name children layers) #:transparent)
(define (make-node name children)
  (node name children '()))
(define (add-layers layers node)
  (struct-copy node ))

;; (define (make-node sym children)
;;   (let ([name (if (pair? sym) (car sym) sym)]
;;         [layers '()])
;;     (vector name children layers)))
;; (define (node-name node)
;;   (vector-ref node 0))
;; (define (node-children node)
;;   (vector-ref node 1))
;; (define (node-layers node)
;;   (vector-ref node 2))
;; (define (add-layers layers node)
;;   (vector (node-name node)
;;           (node-children node)
;;           (append layers (node-layers node))))

;; layers
(define (layer-type layer)
  (second layer))
(define (layer-name layer)
  (third layer))

(layer test process
       (equal?
        (process '(f a b (c 1 2) (layer type a _)))
        #(f (#(a () ((layer type a _)))
             #(b () ())
             #(c (#(1 () ())
                  #(2 () ()))
                 ()))
            ())))

