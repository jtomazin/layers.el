(load "ir")

(define (terminal? o) (or (lit? o) (var? o)))
(define get-children (make-generic-operator 1 'get-children))
(defhandler get-children
  (lambda (exp)
    `(,(app-rator exp)
      ,@(app-rands exp)))
  app?)
(defhandler get-children
  (lambda (exp)
    (lambda-body exp))
  lambda?)
;; (defhandler get-children
;;   (lambda (exp)
;;     )
;;   define?)
(defhandler get-children
  (lambda (exp)
    (list (if-pred exp)
          (if-consq exp)
          (if-alt exp)))
  if?)
;; (defhandler get-children
;;   (lambda (exp)
;;     )
;;   let?)

(define (print-paths paths)
  (for-each
   (lambda (path)
     (display "=== === === === === === === === === ===")
     (newline)
     (print-path path))
   paths))

(define (print-path path)
  (map pp (interpose "---" (map unparse path))))

(define (interpose sep lst)
  (except-last-pair (append-map (lambda (e)
                                  (list e sep))
                                lst)))


(define (accumulate-paths node)
  (if (terminal? node)
      (list (list node))
      (map (lambda (path) 
             (cons node path))
           (append-map accumulate-paths (get-children node)))))


;;; tests
(define program (parse '(lambda (in)
                          ((lambda (f1)
                             (f1 f1 in))
                           (lambda (f n)
                             (if (< n 2)
                                 n
                                 (+ (f f (- n 2))
                                    (f f (- n 1)))))))))
