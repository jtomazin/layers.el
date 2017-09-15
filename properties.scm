;;; table of properties -> propagator cells
(cd "propagator/")
(load "load")
(cd "..")

(define (get table prop)
  (hash-table/get table prop #f))

(define put! hash-table/put!)

(define (make-props-table)
  (let ((props (make-strong-eqv-hash-table)))
    (put! props 'type (make-cell))
    ;; (put! props 'prop (make-cell)) ; more of these
    props))



