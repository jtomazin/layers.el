;;; layers-test.el --- ... -*- lexical-binding: t; -*-

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

(defun layers--apply-expansion ()
  (lambda (code-string)
    (with-output-to-string
      ;; how should this macro be defined?
      (pp (macroexpand (car (read-from-string code-string)))))))

;; these are called by layer-macro. Maybe put them in that file?
(defmacro layers-default-elisp-test-macro (symbol body)
  ;; TODO: use ERT
  (cons 'list
        (-map
         (lambda (expr)
           (if (member '-> expr)        ; (a -> b) special form
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

(defun layers-extract-tests (filename)
  (interactive (list (layers--input-filename)))
  (let ((source-buffer (current-buffer))
        (target-buffer (generate-new-buffer filename))
        (test-layers (nreverse (layers--get-strings-by-label 'test)))
        (spec-layers (nreverse (layers--get-strings-by-label 'spec))))
    ;; put setup boilerplate here
    ;; put spec setup here
    (-each (-map #'layers-apply-expansion test-layers)
      (lambda (test)
        (with-current-buffer target-buffer
          (insert (concat test (string ?\n))))))
    ;; put teardown boilerplate here 
    (with-current-buffer target-buffer
      (write-file filename))))
