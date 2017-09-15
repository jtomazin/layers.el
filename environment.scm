;; Mapping of variables -> props -> cells
;;  The properties for the other expressions live in the AST

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables props)
  (cons variables props))
(define (frame-variables frame) (car frame))
(define (frame-props frame) (cdr frame))
(define (add-binding-to-frame! var props frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons props (cdr frame))))

(define (extend-environment vars props base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars props) base-env)
      (if (< (length vars) (length props))
          (error "Too many arguments supplied" vars props)
          (error "Too few arguments supplied" vars props))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
