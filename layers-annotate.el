;;; layers.el --- Layered Programming in Emacs -*- lexical-binding: t; -*-
;; Layered programming lets users write their code in separate layers within the same
;;  file. For example, you can write tests right alongside your code, and then hide
;;  them away when you don't want to see them anymore. All your tests would
;;  constititute a "layer" within your program.
;;
;; In this document, the 'source' or 'parent' buffer refers to the layered file, and
;;  the 'layer-editing' or just 'editing' buffer refers to the buffer used to
;;  explicitly edit the layers 

;;; To Do:
;; - open file w/ layers
;; - save/modify file w/ layers
;; - read in config
;; - show/hide layers
;; - tangle out layers
;; - deal with cutting/pasting
;; - how to customize?

(require 'cl-lib)

(defvar layer-table '()) ;; should this be buffer local?
;; also this should probably be a hash table

(defvar layers--parent-overlay nil
  "Edit-buffer-local variable to keep track of the edit-buffer's
parent (i.e. the source buffer).")

(defun layer-initialize-buffer ()
  (interactive)
  (setq layer-table '()))

(define-minor-mode layered-mode
  "Minor mode for layered programming in emacs."
  nil " Lyr" nil
  ;; TODO: What happens when you exit layered-mode?
  ;;  Where does your data go?
  ;;  Idea: save it in an a associated file or write
  ;;  to buffer (in a not-very-beautiful way)
  )

(define-minor-mode layer-editing-mode
  "Minor mode for editing the layers in `layered-mode'."
  nil nil nil
  ;; TODO
  )

(defun layers--make-source-overlay (beg end)
  "Make an overlay for a symbol in the source file starting at beg
and ending at end."
  (let ((overlay (make-overlay beg end (current-buffer) t nil))
        (read-only (list
                    (lambda (&rest _)
                      (user-error
                       "Edit the \"name\" field with <placeholder>")))))
    ;; Change modification hook to update data structure instead of
    ;;  making region read-only
    (overlay-put overlay 'face 'underline)
    (overlay-put overlay 'modification-hooks read-only)
    (overlay-put overlay 'insert-in-front-hooks read-only)
    (overlay-put overlay 'insert-behind-hooks read-only)
    (overlay-put overlay 'layer-annotation t) ;; for tracking
    overlay))

(defun layers--get-source-overlay-at-point ()
  (car (cl-remove-if-not (lambda (ol)
                           (overlay-get ol 'layer-annotation))
                         (overlays-at (point)))))

(ert-deftest layers--construct-edit-buffer-name-test ()
  (should (equal "*Layers in layers.el [test]*"
                 (layers--construct-edit-buffer-name "layers.el" "test"))))

(defun layers--construct-edit-buffer-name (orig-buffer-name sym-name)
  "Construct the buffer name for an editing buffer. ORIG-BUFFER-NAME is
the name of the original buffer (usually accessed with (`buffer-name')).
SYM-NAME is the name of the symbol being annotated."
  (concat "*Layers in " orig-buffer-name " [" sym-name "]*"))

(defun layers-add ()
  (interactive)
  (let ((existing-overlay (layers--get-source-overlay-at-point)))
    (if (layers--get-source-overlay-at-point)
        (user-error "Layers already exist for this symbol")
        (let* ((bounds (bounds-of-thing-at-point 'symbol))
               (overlay (layers--make-source-overlay (car bounds) (cdr bounds))))
          (push `(,overlay . (("name" . ,(thing-at-point 'symbol t))
                              ("doc"     . nil)
                              ("body"    . nil)
                              ("spec"    . nil)
                              ("tests"   . nil)))
                layer-table)))))

(defun layers-edit ()
  (interactive)
  (let ((parent-overlay (layers--get-source-overlay-at-point)))
    (if (not parent-overlay) (user-error "No layers exist here"))
    (let*
        ((fields (alist-get parent-overlay layer-table))
         (buffer (generate-new-buffer
                  (layers--construct-edit-buffer-name (buffer-name)
                                                      (cdr (assoc "name" fields)))))
         (major major-mode))
      ;; TODO: other switching configurations    
      ;; TODO: make this manageably editable 
      (switch-to-buffer-other-window buffer)
      (cl-loop for f in fields
               do (layers--insert-field-in-edit-buffer f))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil)
      ;; pulled straight from org-src.el:
      (condition-case e (funcall major)
                      (error (message "Language mode `%s' fails with: %S"
                                      major (nth 1 e))))
      ;; TODO: put in minor mode
      (setq-local layers--parent-overlay parent-overlay))))

(defun layers-edit-abort ()
  "Abort layer editing and return to the source buffer. Abandons changes made in the editing buffer."
  (interactive)
  (unless (layers--edit-buffer-p) (user-error "Not in a layer-editing buffer"))
  ;; TODO 
  )

;; see org-src.el for a good example of how to do this
(defun layers-edit-save ()
  "Write changes in layer-editing buffer back to source buffer and saves the source buffer. (Does not return to source buffer.)"
  ;; TODO: figure out how org saves data, copy that
  (interactive)
  (unless (layers--edit-buffer-p) (user-error "Not in a layer-editing buffer"))
  ;; TODO: clean up edit buffer
  (let ((new-layers (cons layers--parent-overlay
                          (layers--extract-alist-from-buffer (current-buffer))))
        (parent-buffer (overlay-buffer layers--parent-overlay)))
    (with-current-buffer parent-buffer
      (push new-layers layer-table))
    (set-buffer-modified-p nil)))

(defun layers-edit-exit ()
  "Kill current layer-editing buffer and return to source buffer. Modifies source buffer but does not save it."
  (interactive)
  (unless (layers--edit-buffer-p) (user-error "Not in a layer-editing buffer"))
  ;; TODO
  )

(defun layers--edit-buffer-p (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (local-variable-p 'layers--parent-overlay buffer))))

(defun layers-remove ()
  (interactive)
  (let ((overlay (layers--get-source-overlay-at-point)))
    (if (null overlay)
        (user-error "Can't find symbol in layer table"))
    (delete-overlay overlay)
    (setq layer-table (cl-remove-if (lambda (e) (eq (car e) overlay))
                                    layer-table))))

(defun layers--insert-field-in-edit-buffer (field &optional buffer)
  "Inserts FIELD into BUFFER. FIELD should be of the form (HEADER . VALUE), 
where HEADER is the name of the field and VALUE is it's associated value. 
If BUFFER is not provided, then it defaults to the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        (header (format "%s:" (car field)))
        (value (format "%s" (cdr field))))
    (with-current-buffer buffer
                         (let ((m1 (make-marker))
                               (m2 (make-marker)))
                           (set-marker m1 (point))
                           (insert header)
                           (set-marker m2 (point))
                           (newline)
                           (insert value)
                           (newline)
                           (newline)
                           (layers--make-field-header-overlay m1 m2)))))

(defun layers--extract-alist-from-buffer (buffer)
  (with-current-buffer
   buffer
   (let* ((overlays
           (sort (cl-remove-if-not
                  (lambda (ol) (overlay-get ol 'layer-header))
                  (overlays-in 1 (point-max)))
                 (lambda (o1 o2) (< (overlay-start o1) (overlay-start o2)))))
          (overlay-bounds
           (mapcar (lambda (ol) (list (overlay-start ol) (overlay-end ol)))
                   overlays))
          ;; triples of form (M1 M2 M3) where M1 and M2 denote the position of the
          ;;   key and M2 and M3 denote the position of the value
          (triples
           (cl-mapcar 'append
                      overlay-bounds
                      (append (mapcar (lambda (l) (list (car l)))
                                      (cdr overlay-bounds))
                              (list (list (point-max))))))
          ;; get text without properties or whitespace
          (mapcar (lambda (triple)
                    (cl-destructuring-bind (m1 m2 m3) triple
                                           (cons (layers--chomp
                                                  (buffer-substring-no-properties
                                                   m1 (1- m2)))
                                                 (layers--chomp
                                                  (buffer-substring-no-properties
                                                   m2 m3)))))
                  triples)))))

(defun layers--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun layers--make-field-header-overlay (beg end)
  (let ((overlay (make-overlay beg end (current-buffer) t nil))
        (read-only (list
                    (lambda (&rest _)
                      (user-error
                       "These headers can be modified by changing your config")))))
    (overlay-put overlay 'face 'italic)
    (overlay-put overlay 'modification-hooks read-only)
    ;;(overlay-put overlay 'insert-in-front-hooks read-only)
    ;;(overlay-put overlay 'insert-behind-hooks read-only)
    (overlay-put overlay 'layer-header t) ;; for tracking
    overlay))

;; (defun layers--compose (&rest fns)
;;   "Return function composed of FNS."
;;   (lambda (&rest args)
;;     (reduce 'funcall (butlast fns)
;;             :from-end t
;;             :initial-value (apply (car (last fns)) args))))
