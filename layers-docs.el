;;; layers-docs.el -*- lexical-binding: t; -*-
(layer doc package
  "This module performs the extraction and organization of documentation
in a layered file. This is where it would be appropriate to give a high
level overview of the components of this module and its interfaces."

  "This module analyzes the file/directory structure of the code, establishes
a heirarchy of modules, and then assembles the documentation embedded in those
modules.")

(require 'dash)

(defun vertex (name children)
  [name children])
(defun vertex-name (vertex)
  (seq-elt vertex 0))
(defun vertex-children (vertex)
  (seq-elt vertex 1))

(defun make-heirarchy (entry-point) 
  (vertex (module-name entry-point)
          (-map #'make-heirarchy
                (dependencies entry-point))))
