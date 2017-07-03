;;; layers-overlays.el --- Manage overlays -*- lexical-binding: t; -*-
;; This file manages the construction and manipulation of overlays
;; for embedding layers in a layered file.

(require 'dash)

(require 'layers-macro)

(defvar layer-labels (list "test" "doc" "spec")
  "the names of all the labels of layers")

(defcustom layers-colored-summaries nil
  "Hidden layer summaries are colorized if this is non-nil; otherwise
they are formatted as comments.")

(defconst layers--macro-regex "\(layer[[:space:]\n]")

(layer test layers--macro-regex
  (string-match layers--macro-regex "(layer ")
  (string-match layers--macro-regex "(layer test")
  (string-match layers--macro-regex "(layer
test")
  (string-match layers--macro-regex "(layer
")
  (string-match layers--macro-regex "(layer    name")
  (not (string-match layers--macro-regex "(layers"))
  (not (string-match layers--macro-regex "(layer"))
  (not (string-match layers--macro-regex "layers"))
  (not (string-match layers--macro-regex "(defun layer"))
  (not (string-match layers--macro-regex "layer")))

(defun layers--label (label)
  "Return the symbol associated with LABEL's layer (e.g., \"test\")"
  (if (stringp label)
      (intern label)
    label))

(layer test layers--label
  ("test" -> 'test)
  ('test  -> 'test)
  (eq (layers--label "test") (layers--label 'test))
  (eq (layers--label "test") (layers--label "test")))

(defun layers--get-overlays-by-label (label)
  (-filter (lambda (ol)
             (eq (overlay-get ol 'layer-label) (layers--label label)))
           (overlays-in (point-min) (point-max))))

(defun layers--get-strings-by-label (label)
  (-map (lambda (ol)
          (buffer-substring-no-properties (overlay-start ol) (overlay-end ol)))
        (-filter (lambda (ol)
                   (eq (overlay-get ol 'layer-label) (layers--label label)))
                 (overlays-in (point-min) (point-max)))))

(defun layers--get-nth-sexpr (beg n)
  "Returns a string of the N-th s-expression in the s-expression
starting at BEG. Zero-indexed."
  (let ((end (scan-sexps beg 1)))
    (symbol-name (nth n (car (read-from-string
                              (buffer-substring beg end)))))))

(layer test layers--get-nth-sexpr
  (-map (lambda (ol) (layers--get-nth-sexpr (overlay-start ol) 2))
        (layers--get-overlays-by-label 'test)))

(defun layers--get-macro-symbol (pos-or-overlay)
  "Gets the symbol associated with the layer macro specified by
POS-OR-OVERLAY. POS-OR-OVERLAY can be a character position specifying
the start of the layer sexpr, or the layer overlay covering it."
  (let ((beg (cond ((overlayp pos-or-overlay) (overlay-start pos-or-overlay))
                   ((integerp pos-or-overlay) pos-or-overlay)
                   ((markerp pos-or-overlay) (marker-position pos-or-overlay))
                   (t (error "Got %s; expected int or overlay"
                             (type-of pos-or-overlay))))))
    (layers--get-nth-sexpr beg 2)))

(layer test layers--get-macro-symbol
  (condition-case e
      (layers--get-macro-symbol "illegal")
    (error (string-match-p "string" (cadr e))))
  (condition-case e
      (layers--get-macro-symbol 'illegal)
    (error (string-match-p "symbol" (cadr e)))))

(defun layers--put-overlay-on-sexpr (beg)
  "Place a labeled layer overlay on the `layer' macro starting at position
BEG, or update it with a new label. Does nothing if a layer overlay
of the correct type is already there."
  (let ((other-ols (overlays-at beg))
        (end (scan-sexps beg 1))
        (label (layers--label (layers--get-nth-sexpr beg 1))))
    (let ((overlay (or (-some (lambda (ol) ; overlay exists?
                                (and (= beg (overlay-start ol))
                                     (= end (overlay-end ol))
                                     (overlay-get ol 'layer-label)
                                     ol))
                              other-ols)
                       (make-overlay beg (scan-sexps beg 1) ; or make a new one
                                     (current-buffer) t nil))))
      (overlay-put overlay 'layer-label label)
      overlay)))

(defun layers--input-label ()
  (layers--label (completing-read "Layer: " layer-labels)))

(defun layers--construct-summary-string (label symbol)
  (let* ((->string (lambda (var) (if (stringp var) var
                                   (symbol-name var))))
         (label (funcall ->string label))
         (symbol (funcall ->string symbol)))
    (if layers-colored-summaries
        (concat (propertize  "(" 'font-lock-face 'italic)
                (propertize label 'font-lock-face
                            '(italic font-lock-keyword-face))
                " "
                (propertize symbol 'font-lock-face
                            '(italic font-lock-function-name-face))
                (propertize  " ...)" 'font-lock-face 'italic))
      (propertize (concat "(" label " "
                          symbol " ...)")
                  'font-lock-face 'font-lock-comment-face))))

(defun layers-hide-layer (label)
  ;; (interactive (layers--input-label)) 
  (-each (layers--get-overlays-by-label label)
    (lambda (ol)
      (let ((symbol (layers--get-macro-symbol ol)))
        (overlay-put ol 'invisible t)
        (overlay-put ol 'after-string
                     (layers--construct-summary-string label symbol))))))

(defun layers-show-layer (label)
  ;; (interactive (layers--input-label))
  (-each (layers--get-overlays-by-label label)
    (lambda (ol)
      (overlay-put ol 'invisible nil)
      (overlay-put ol 'after-string nil))))

(defun layers-toggle-layer (label)
  (interactive (list (layers--input-label)))
  (let* ((ol (car (layers--get-overlays-by-label label)))
         (invisible (overlay-get ol 'invisible)))
    (cond
     ((null ol) (message "No layers of type %s" label))
     (invisible (layers-show-layer label))
     (t         (layers-hide-layer label)))))

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

(defun layers--update-overlays (&rest _)
  ;; todo: locality optimization
  (-each (layers--find-layers)
    #'layers--put-overlay-on-sexpr))

(provide 'layers-overlays)
