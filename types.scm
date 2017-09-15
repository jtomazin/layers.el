;;; Representation of Types

;; Thought: Should parametric types have cells for each type they're
;; parameterized by?

;; There are two distinctions of types: primitive types, like numbers
;; and strings, and parametric types, like procedures and lists

;; Primitive types
(define numeric-type (list 'type:numeric))
(define (numeric-type? o) (eq? numeric-type))

(define boolean-type (list 'type:boolean))
(define (boolean-type? o) (eq? boolean-type))

(define symbol-type (list 'type:symbol))
(define (symbol-type? o) (eq? symbol-type))


;; Parametric Types
(define parametric-type-operators '())

(define (define-parametric-type-operator operator)
  ;; (guarantee symbol? operator)
  (set! parametric-type-operators
        (cons operator parametric-type-operators))
  (values (lambda operands
            (cons operator operands))
          (parametric-type-predicate operator)))

(define (parametric-type-operator param)
  (car param))

(define (parametric-type-operands param)
  (cdr param))

(define ((parametric-type-predicate operator) expr)
  (and (parametric-type? expr)
       (eq? (parametric-type-operator expr) operator)))

(define procedure-type)
(define procedure-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:procedure)
  (set! procedure-type constructor)
  (set! procedure-type? predicate))

(define (procedure-type-domains expr)
  (car (parametric-type-operands expr)))

(define (procedure-type-codomain expr)
  (cadr (parametric-type-operands expr)))

(let ((v numeric-type))
  (equal? (procedure-type (list v v) v)
          (procedure-type (list v v) v)))
