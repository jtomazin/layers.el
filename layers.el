;;; layers.el --- Layered Programming in Emacs -*- lexical-binding: t; -*-

(require 'dash)

(define-minor-mode layered-mode
  "Minor mode for layered programming in emacs."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l") #'layers-toggle-layer)
            map)
  :lighter " Lyr"
  (if layered-mode
      (layers--initialize)
    (layers--tear-down)))

(defvar layer-labels (list)
  "the names of all the labels of layers")

;; This macro is the basic construct of layered programs.
;;  A user intersperses her program with these decorations
;;  which contain extra information about the program which
;;  is not necessary for execution (e.g., tests, types,
;;  extra documentation, etc.)
(defmacro layer (name &rest body)
  "doc"
  (declare (indent 1)) 
  (case name
    (test `(list ,@body))
    (t    nil)))

(defconst layers--macro-regex "\(layer[[:space:]\n]")

;; (layer test example-fn
;;   ("a" -> 1)
;;   ("b" -> 2)
;;   ("c" -> 3))

;; (layer test
;;   (string-match layers--macro-regex "(layer ")
;;   (string-match layers--macro-regex "(layer test")
;;   (string-match layers--macro-regex "(layer
;; test")
;;   (string-match layers--macro-regex "(layer
;; ")
;;   (string-match layers--macro-regex "(layer    name")
;;   (not (string-match layers--macro-regex "(layers"))
;;   (not (string-match layers--macro-regex "(layer"))
;;   (not (string-match layers--macro-regex "layers"))
;;   (not (string-match layers--macro-regex "(defun layer"))
;;   (not (string-match layers--macro-regex "layer")))

(defun layers--get-nth-sexpr (pos n)
  "Returns the string of the N-th s-expression in the
s-expression starting at POS. 1-indexed."
  (let ((beg (1+ pos)))
    (if (= n 1)
        (buffer-substring-no-properties pos
                                        (scan-sexps beg n))
      (buffer-substring-no-properties (1+ (scan-sexps beg (- n 1)))
                                      (scan-sexps beg n)))))

(defun layers--label (label)
  "Return the symbol associated with LABEL's layer (e.g., \"test\")"
  (intern (concat (if (stringp label)
                      label
                    (symbol-name label))
                  "-layer")))

(layer test
  (eq (layers--label "test") 'test-layer)
  (eq (layers--label "test") (layers--label 'test))
  (eq (layers--label "test") (layers--label "test"))
  (eq (layers--label 'test) (layers--label 'test)))

(defun layers--put-overlay-on-sexpr (beg)
  "Place a labeled layer overlay on the `layer' macro starting at position
BEG, or update it with a new label. Does nothing if a layer overlay
of the correct type is already there."
  (let ((other-ols (overlays-at beg))
        (end (scan-sexps beg 1))
        (label (layers--label (layers--get-nth-sexpr beg 2))))
    (let ((overlay (or (-some (lambda (ol) ; overlay exists?
                                (and (= beg (overlay-start ol))
                                     (= end (overlay-end ol))
                                     (overlay-get ol 'layer-label)
                                     ol))
                              other-ols)
                       (make-overlay beg (scan-sexps beg 1) ; or make a new one
                                     (current-buffer) t nil))))
      (overlay-put overlay 'layer-label label))))

(defun layers--get-layers-by-label (label)
  (-filter (lambda (ol)
             (eq (overlay-get ol 'layer-label) (layers--label label)))
           (overlays-in 1 (point-max))))

(defun layers--input-label ()
  (completing-read "Layer: " layer-labels))

(defun layers-hide-layer (label)
  ;; (interactive (layers--input-label))
  (let ((label (if (stringp label)
                   (intern label)
                 label)))
    (-each (layers--get-layers-by-label label)
      (lambda (ol)
        (overlay-put ol 'invisible t)
        (overlay-put ol 'before-string
                     (concat "(" (symbol-name label) "...)"))))))

(defun layers-show-layer (label)
  ;; (interactive (layers--input-label))
  (let ((label (if (stringp label)
                   (intern label)
                 label)))
    (-each (layers--get-layers-by-label label)
      (lambda (ol)
        (overlay-put ol 'invisible nil)
        (overlay-put ol 'before-string nil)))))

(defun layers-toggle-layer (label)
  (interactive (list (layers--input-label)))
  (let ((label (if (stringp label)
                   (intern label)
                 label)))
    (if (overlay-get (car (layers--get-layers-by-label label))
                     'invisible)
        (layers-show-layer label)
      (layers-hide-layer label))))

(defun layers--find-layers ()
  "Return the character positions marking the beginning of all
layer macros in the buffer."
  (save-excursion
    (goto-char 1)
    (let ((layer-positions '()))
      (while (re-search-forward layers--macro-regex nil 0)
        (let ((beg (match-beginning 0)))
          ;; TODO: this could be optimized
          (unless (syntax-ppss-context (syntax-ppss (point)))
            (push beg layer-positions))))
      layer-positions)))

(layers--find-layers)

(defun layers--update-overlays (&rest _)
  (-each (layers--find-layers)
    #'layers--put-overlay-on-sexpr))

(defun layers--initialize ()
  (add-hook 'after-change-functions #'layers--update-overlays t t)
  (layers--update-overlays))

(defun layers--tear-down ()
  (remove-hook 'after-change-functions #'layers--update-overlays t))

;; (layers--tear-down)
;; (layers--initialize)
