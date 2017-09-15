;;; Mess around with type inference using propagators 
(cd "propagator")
(load "load")
(cd "..")
;; (load "types")
(load "unify")
(load "ghelper")

;;; Propagator Paradigms:
;;   As far as I can tell, there are two distinct ways to compute with
;; propagators. One is the partial information paradigm, in which
;; cells contain somewhere between no information to full information,
;; and propagators provide and propagate (duh) information. The other
;; is the dependency-directed implicit search/constraint resolution,
;; in which cells carry the range of possible values a variable might
;; have, and propagators representing the constraints of the system
;; constrain these cells down. In the former paradigm, we go from
;; nothing -> something, and in the latter, we go from everything ->
;; something.

;;  The partial information paradigm is substantially simpler.
;; However, I suspect the search paradigm is more powerful, as its
;; cells to contain a range of values rather than a single one (as is
;; often the case in dynamic languages) and might help get around the
;; limitations of the "monomorphic" type system that Radul discusses
;; in his PhD thesis (p101). The first obvious difficulty is that each
;; cell must enumerate every possible type it may be. Taking a naive
;; approach, this explodes because of parametric types: a procedure
;; could be (int) -> int, (int int) -> int, (int int int) -> int, etc,
;; for every combination of types. Clearly straightforward enumeration
;; will not work. Can this be solved with smarter type variables? (yes)

;; GJS: don't think about the cells as representing a type (or a
;; value); think about cells as a place where you accumulate
;; information about the type. Specifically, use type variables, which
;; will evolve over time. Also:
;;      "Confusion is the first step towards enlightenment."

;;; Code 
(define generate-type-name
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (symbol 'type ":" n))))

;; (define-record-type <type-var>
;;   (%make-type-var name)
;;   type-var?
;;   (name tvar-name))
;; (define (make-type-var)
;;   (%make-type-var (generate-type-name)))
(define (make-type-var)
  `(? ,(generate-type-name)))

;; (define (type-var? object)
;;   (and (pair? object)
;;        (eq? (car object) '?)
;;        (pair? (cdr object))
;;        (symbol? (cadr object))
;;        (or (null? (cddr object))
;;            (and (pair? (cddr object))
;;                 (symbol? (caddr object))
;;                 (null? (cdddr object))))))

;; (define (type-test)
;;   (let ((app-type (make-cell))
;;         (fn-type  (make-cell))
;;         (arg-type (make-cell))
;;         (o (make-cell)))
;;     (one-of all-types app-type)
;;     (one-of all-types fn-type)
;;     (one-of all-types arg-type)
;;     ((constant numeric-type) app-type)
;;     (content app-type)))

;; (defhandler merge
;;   (lambda (content increment)
;;     ;; next: unify
;;     ;; for now: 
;;     increment)
;;   type-var? )

(define pattern? list?)
(define pattern-merge (make-generic-operator 2 'pattern-merge))
(defhandler pattern-merge
  (lambda (content increment)
    increment)
  nothing? any?)
(defhandler pattern-merge
  unifier
  pattern? pattern?)

;;; 
(define (eq-prop-test)
  (let ((app (make-cell pattern-merge))
        (fn (make-cell pattern-merge))
        (arg (make-cell pattern-merge)))
    (initialize-scheduler)
    (add-content app (make-type-var))
    (add-content fn `(,(make-type-var) -> ,(make-type-var)))
    (add-content arg (make-type-var))
    ;; (pp (map content (list c1 c2 c3)))
    (=output-of app fn)
    (=input-of arg fn)
    ;; ((constant (make-type-var)) c1) 
    (run)
    (map content (list app fn arg))))

(define (=output-of app fn)
  (let ((dict (unify (content app) (last (content fn)))))
    (add-content app (unify:value (content app) dict))
    (add-content fn (unify:value (content fn) dict))))
(define (=input-of arg fn)              ; TODO: allow multiple args
  (let ((dict (unify (content arg) (car (content fn)))))
    (add-content arg (unify:value (content arg) dict))
    (add-content fn (unify:value (content fn) dict))))

;; opt1: (c:== c1 (output-of c2))
;; opt2: (=output-of c1 c2)

;; (define (get-return-type cell)
;;   (compound-propagator
;;    (list cell)
;;    (lambda ()
;;      )))

;; need a propagator to say "the type of app is the output type of fn"

