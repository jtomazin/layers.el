;;; layers-code-folding.el --- Layered Programming in Emacs -*- lexical-binding: t; -*-
;; Layers as code-folding; quick sketch:
;; - label a bit of code as belonging to a layer
;; - toggle the visibility of that entire layer
;; - use the minibuffer for options?

(require 'dash)
(require 'auto-overlay-flat)

(defun layers-mark-layer (beginning end layer-name)
  "Mark the region as belonging to LAYER-NAME"
  (interactive
   (let ((layer (read-string "Layer: " nil)))
     (list (region-beginning) (region-end) layer)))
  (let ((overlay (make-overlay beginning end (current-buffer))))
    (overlay-put overlay 'layer layer-name))
  (setq deactivate-mark t))

(defun layers--get-overlay-at-point ()
  ""
  (car (-filter (lambda (ol)
                  (overlay-get ol 'layer))
                (overlays-at (point)))))

(defun layers-show-all ())

(defun layers-hide-layer (layer)
  (-each (get-parts-of-layer layer)
    (lambda (part)
      (hide part))))

(defun layers-show-layer ())

(defun layers-toggle-layer ())

(defun layers-remove ()
  ""
  (interactive)
  (delete-overlay (layers--get-overlay-at-point)))
