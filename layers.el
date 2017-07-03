;;; layers.el --- Layered Programming in Emacs -*- lexical-binding: t; -*-

;; fixme: this breaks `query-replace'
;; todo: doc generation (later)
;; todo: combine specs and tests
;; todo: leverage semantic?
;; todo: better test output
;; todo: plug into test gen library w/ specs?

(require 'dash)

(require 'layers-overlays)
(require 'layers-macro)

(define-minor-mode layered-mode
  "Minor mode for layered programming in emacs."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l") #'layers-toggle-layer)
            map)
  :lighter " Lyr"
  (if layered-mode
      (layers--initialize)
    (layers--tear-down)))

(defcustom layers-check-spec t
  "Check specs on each invocation if this is non-nil. This will have
a significant performance impact.")

;; example:
(defun simplefun (str num)
  (make-list num str))

(simplefun 'a 3)

(layer spec simplefun
  ((stringp integerp) -> listp))

;; (defun p-or (&rest preds)
;;   (lambda (e)
;;     (-any? #'identity (-map (lambda (pred)
;;                               (funcall pred e))
;;                             preds))))

;; (layer spec layers--get-layers-by-label
;;   (((p-or stringp symbolp)) -> (overlayp ...))
;;   (lambda (r)
;;     (-each r
;;       (lambda (ol)
;;         (overlay-get 'layer)))))

(defun layers--initialize ()
  (add-hook 'after-change-functions #'layers--update-overlays t t)
  (layers--update-overlays)
  'done)

(defun layers--tear-down ()
  (remove-hook 'after-change-functions #'layers--update-overlays t)
  (-each (-filter (lambda (ol)
                    (overlay-get ol 'layer-label))
                  (overlays-in (point-min) (point-max)))
    (lambda (ol)
      (delete-overlay ol)))
  'done)

(defun layers--input-filename ()
  (read-from-minibuffer "Write to file: "))


