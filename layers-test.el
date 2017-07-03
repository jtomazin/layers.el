;;; layers-test.el --- ... -*- lexical-binding: t; -*-

;; grab specs and tests -> expand them with defined macros
;;  -> put them together in a file

;; general idea:
;;  - command: extract-layers test
;;  - prompt for macro or default
;;  - expand layers with given macro and put them in a file

;; for elisp: plug into ERT
;; for clojure: plug into clojure.test
;; scheme: ad-hoc?

(require 'layers-overlays)

(defcustom layers-test-macro 'layers-elisp-default-test-macro
  "Macro that maps the contents of a test layer to the expanded tests
for the function.")

(defcustom layers-spec-macro 'layers-elisp-default-spec-macro)

;; (defun layers-tangle-layer (label filename)
;;   (interactive (list (layers--input-label) (layers--input-filename)))
;;   (write-region (apply #'concat
;;                        (-interpose
;;                         (string ?\n)
;;                         (-map (lambda (ol)
;;                                 (let ((beg (overlay-start ol))
;;                                       (end (overlay-end ol)))
;;                                   (with-output-to-string
;;                                     (pp
;;                                      (macroexpand
;;                                       (car (read-from-string
;;                                             (buffer-substring beg end))))))))
;;                               (nreverse (layers--get-overlays-by-label label)))))
;;                 nil filename))

(defun layers--apply-expansion ()
  (lambda (code-string)
    (with-output-to-string
      ;; how should this macro be defined?
      (pp (macroexpand (car (read-from-string code-string)))))))

(defmacro layers-default-elisp-test-macro (symbol body)
  (cons 'list
        (-map
         (lambda (expr)
           (if (member '-> expr) ; (a -> b) special form
               (let ((given (first expr))
                     (expected (car (last expr))))
                 `(not (assert (equal ,expected
                                      (funcall #',symbol ,given)))))
             expr))
         body)))

(defun layers-elisp-default-spec-macro (symbol body)
  ;; plug into generative testing library?
  nil)

(defun layers-elisp-spec-macro-runtime-checking (symbol body)
  (let* ((signature (car body)) ; clojure could use metadata for this
         (arg-spec (car signature))
         (r-spec (car (last signature))))
    `(progn
       (advice-remove ',symbol 'aspec)
       (advice-remove ',symbol 'bspec)
       (advice-add ',symbol :before
                   (lambda (&rest args)
                     (if layers-check-spec
                         (-each (-zip '(,@arg-spec) args)
                           (lambda (p)
                             (assert (funcall (car p) (cdr p)) nil
                                     "Expected %s, got %s of type %s"
                                     (car p) (cdr p) (type-of (cdr p)))))))
                   '((name . bspec)))
       (advice-add ',symbol :filter-return
                   (lambda (r)
                     (if layers-check-spec
                         (assert (funcall #',r-spec r) nil
                                 "Expected %s, got %s of type %s"
                                 ',r-spec r (type-of r)))
                     r)
                   '((name . aspec))))))

;; return expanded strings?
(defun layers-extract-tests (filename)
  (interactive (list (layers--input-filename)))
  (let ((source-buffer (current-buffer))
        (target-buffer (generate-new-buffer filename)))
    ;; put setup boilerplate here
    (-each (-map #'layers-apply-expansion
                 (nreverse (layers--get-strings-by-label label)))
      (lambda (test)
        (with-current-buffer target-buffer
          (insert (concat test (string ?\n))))))
    ;; put teardown boilerplate here 
    (with-current-buffer target-buffer
      (write-file filename))))
