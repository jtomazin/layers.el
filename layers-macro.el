;;; layers-macro.el -*- lexical-binding: t; -*-
;; Commentary:
;; This macro is the basic construct of layered programs.
;;  A user intersperses her program with these decorations
;;  which contain extra information about the program which
;;  is not necessary for execution (e.g., tests, types,
;;  extra documentation, etc.)
;;
;; Code:

(require 'layers-test)

(defmacro layer (name symbol &rest body)
  "doc"
  (declare (indent 2))
  (case name
    (test (layers-test-macro symbol body))
    (doc  (layers-doc-macro symbol body))
    (spec (layers-spec-macro symbol body))
    (t nil)))

(provide 'layers-macro)
