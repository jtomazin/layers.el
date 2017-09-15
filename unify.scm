;;; Unifier from Ch 3 (Pattern Matching) of Jerry Sussman and Chris
;;; Hanson's new book

;; TODO: change vars from lists to records so that the default
;; definitions of MERGE doesn't clobber them?
;;  - Actually, the problem isn't that variables are lists; the
;;  problem is that patterns are lists. Actually, is that true? Do
;;  patterns play nicely with the existing merge handler for pairs?
;;  See compound-data.scm in propagators to see. 

(declare (usual-integrations))

;;; Overview 
;; Walk the common structure of the input patterns. If a variable is
;; encountered on either side of the match it is bound to the data on
;; the other side. When a variable is encountered, it is eliminated
;; from both the accumulated dictionary and the equations remaining to
;; be solved. UNIFY:VALUE substitutes known variables into one of the
;; patterns to give the unified expression.

(define (unifier x y)
  (let ((dict (unify x y)))
    (and dict
         (unify:value x dict))))

(define (unify t1 t2)
  (unify:internal t1 t2 (unify:new-dict) (lambda (dict) dict)))

(define (unify:internal p1 p2 dict succeed)
  (unify:loop
   (make-ustate (list p1) (list p2) dict '() succeed)))

;;; Performing Unification 
;; The unification program is organized as a state machine with state
;; variables XS and YS holding the elements to be matched at the
;; current level of structure. SAVED-LEVELS contains material that
;; will be analyzed after the current level is exhausted. SUCCEED
;; continuation is invoked if the match finishes successfully.

;; The state:
(define-record-type <unify-state>
  (make-ustate xs ys dict saved-levels succeed)
  unify-state?
  (xs ustate:xs)
  (ys ustate:ys)
  (dict ustate:dict)
  (saved-levels ustate:saved-levels)
  (succeed ustate:succeed))

;; Step forward along the current level
(define (ustate:replace-level xs ys state)
  (make-ustate xs
               ys
               (ustate:dict state)
               (ustate:saved-levels state)
               (ustate:succeed state)))

;; Step down into the next level of matching and save the current
;; level
(define (ustate:new-level xs* ys* state)
  (make-ustate xs*
               ys*
               (ustate:dict state)
               (cons (cons (ustate:xs state)
                           (ustate:ys state))
                     (ustate:saved-levels state))
               (ustate:succeed state)))

;; Step up back into the previous saved level
(define (ustate:resume-level state)
  (make-ustate (caar (ustate:saved-levels state))
               (cdar (ustate:saved-levels state))
               (ustate:dict state)
               (cdr (ustate:saved-levels state))
               (ustate:succeed state)))

;; And the state machine. This unifier is likely unique in that it can
;; handle segment variables.
(define (unify:loop state)
  (let ((xs (ustate:xs state))
        (ys (ustate:ys state)))
    (cond ((and (pair? xs) (pair? ys))
           (let ((x (car xs))
                 (y (car ys))
                 (state*
                  (ustate:replace-level (cdr xs)
                                        (cdr ys)
                                        state)))
             (cond ((or (unify:element-variable? x)
                        (unify:element-variable? y))
                    (if (or (unify:segment-variable? x)
                            (unify:segment-variable? y))
                        #f
                        (handle-element-variables x y state*)))
                   ((or (unify:segment-variable? x)
                        (unify:segment-variable? y))
                    (handle-segment-variables x y state*))
                   ((and (list? x) (list? y))
                    (unify:loop (ustate:new-level x y state*)))
                   ((eqv? x y)          ; they're both concrete data
                    (unify:loop state*))
                   (else #f))))
          ((and (null? xs) (null? ys))
           (if (pair? (ustate:saved-levels state))
               (unify:loop (ustate:resume-level state))
               ((ustate:succeed state) (ustate:dict state))))
          (else #f))))

;; Handling element variables:
(define (handle-element-variables x y state)
  (cond ((not (unify:element-variable? x))
         (maybe-eliminate-variable y x state))
        ((not (unify:element-variable? y))
         (maybe-eliminate-variable x y state))
        ((equal-vars? x y)
         (unify:loop state))
        (else
         (eliminate-variable x y state))))
;; If the data that would be used to eliminate a variable contains a
;; reference to said variable (e.g., eliminating (? a) with (1 (? a) 3)), then
;; the match cannot procede. This is called the "occurs check".
(define (maybe-eliminate-variable var target state)
  (and (not (unify:occurs-in? var target))
       (eliminate-variable var target state)))

(define (eliminate-variable var target state)
  (unify:loop
   (let ((subst (unify:substitution var target)))
     (make-ustate (subst (ustate:xs state))
                  (subst (ustate:ys state))
                  (extend-dict var target
                               (map-dict-values subst
                                                (ustate:dict state)))
                  (map (lambda (p)
                         (cons (subst (car p))
                               (subst (cdr p))))
                       (ustate:saved-levels state))
                  (ustate:succeed state)))))

(define (unify:substitution var target)
  (lambda (pattern)
    (map-pattern-vars (lambda (var*)
                        (equal-vars? var* var))
                      (lambda (var*)
                        target)
                      pattern)))

;; Handling segment variables. Backtracking works by trying to bind
;; the largest possible segment, and successively trying smaller
;; segments as it fails. If segment becomes less than empty, then the
;; failure propagates to the previous segment variable match, or to
;; the top level.
(define (handle-segment-variables x y state)
  (define (do-it x xs ys)
    (let slp ((n (length ys)))
      (and (>= n 0)
           (or (maybe-eliminate-variable
                x
                (list-head ys n)
                (ustate:replace-level xs (list-tail ys n) state))
               (slp (- n 1))))))
  (if (unify:segment-variable? x)
      (do-it x
             (ustate:xs state)
             (cons y (ustate:ys state)))
      (do-it y
             (ustate:ys state)
             (cons x (ustate:xs state)))))

;; The "occurs check". Checks if var occurs in pattern.
(define (unify:occurs-in? var pattern)
  (let lp ((pattern pattern))
    (cond ((unify:variable? pattern) (equal-vars? pattern var))
          ((list? pattern) (any lp pattern))
          (else #f))))


;;; Insantiating the Pattern
;; This is how the dictionary is used to instantiate a pattern:
(define (unify:value pattern dict)
  (map-pattern-vars (lambda (var)
                      (unify:lookup var dict))
                    (lambda (var)
                      (unify:content (unify:lookup var dict)))
                    pattern))


;;; Variable Representation:
(define ((unify:variable-predicate marker) object)
  (and (pair? object)
       (eq? marker (car object))
       (pair? (cdr object))
       (symbol? (cadr object))
       (null? (cddr object))))

(define unify:unrestricted-element-variable?
  (unify:variable-predicate '?))

(define unify:unrestricted-segment-variable?
  (unify:variable-predicate '??))

(define ((unify:restricted-variable-predicate marker) object)
  (and (pair? object)
       (eq? marker (car object))
       (pair? (cdr object))
       (symbol? (cadr object))
       (pair? (cddr object))
       (procedure? (caddr object))
       (null? (cdddr object))))

(define unify:restricted-element-variable?
  (unify:restricted-variable-predicate '?))

(define unify:restricted-segment-variable?
  (unify:restricted-variable-predicate '??))

(define (unify:element-variable? var)
  (or (unify:unrestricted-element-variable? var)
      (unify:restricted-element-variable? var)))

(define (unify:segment-variable? var)
  (or (unify:unrestricted-segment-variable? var)
      (unify:restricted-segment-variable? var)))

(define (unify:restricted-variable? var)
  (or (unify:restricted-element-variable? var)
      (unify:restricted-segment-variable? var)))

(define (unify:variable? o)
  (or (unify:element-variable? o)
      (unify:segment-variable? o)))

(define (unify:var-type var)
  (car var))

(define (unify:name var)
  (cadr var))

(define (unify:restriction var)
  (caddr var))

(define (same-type-vars? v1 v2)
  (eq? (unify:var-type v1)
       (unify:var-type v2)))

(define (equal-vars? v1 v2)
  (and (same-type-vars? v1 v2)
       (eqv? (unify:name v1) (unify:name v2))))


;;; Dictionary:
(define (unify:new-dict)
  '())

(define (extend-dict var target dict)
  (cons (list (unify:name var) target (unify:var-type var))
        dict))

(define (map-dict-values procedure dict)
  (map (lambda (entry)
         (list (car entry)
               (procedure (cadr entry))
               (caddr entry)))
       dict))

(define (unify:lookup var dict)
  (let ((b (assq (unify:name var) dict)))
    (if (and b
             (pair? (cddr b))
             (not (eq? (unify:var-type var) (caddr b))))
        (error "Mismatched var types:" var b))
    b))

(define (unify:content vcell)
  (cadr vcell))

(define (unify:bind var target dict)
  (extend-dict var target
               (map-dict-values (let ((ndict
                                       (extend-dict var target
                                                    (unify:new-dict))))
                                  (lambda (target*)
                                    (unify:value target* ndict)))
                                dict)))


;;; Syntax
;; Applies PROCEDURE to each VAR-INTERESTING? in PATTERN
(define (map-pattern-vars var-interesting? procedure pattern)
  (let elp ((pattern pattern))
    (cond ((unify:element-variable? pattern)
           (if (var-interesting? pattern)
               (procedure pattern)
               pattern))
          ((unify:segment-variable? pattern)
           (if (var-interesting? pattern)
               (error "should not get here"))
           pattern)
          ((list? pattern)
           (append-map (lambda (pattern*)
                         (if (unify:segment-variable? pattern*)
                             (if (var-interesting? pattern*)
                                 (procedure pattern*)
                                 (list pattern*))
                             (list (elp pattern*))))
                       pattern))
          (else pattern))))
