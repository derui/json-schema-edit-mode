;;; json-schema-edit-xref.el --- xref and eldoc for $ref  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; An xref backend for JSON Schema: `M-.' on a `$ref' jumps to its
;; definition, and `M-?' on a definition (or on a `$ref') lists every
;; `$ref' that points to it, in the current file and in the schema files
;; next to it.  An eldoc function shows a summary of the `$ref' target.

;;; Code:

(require 'xref)
(require 'eldoc)
(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-header)

(defcustom json-schema-edit-reference-files-function
  #'json-schema-edit-xref-sibling-files
  "Function returning the files searched for references by `M-?'.
It is called in the schema buffer with no arguments and returns a list
of absolute file names.  The current file is always searched as well."
  :type 'function
  :group 'json-schema-edit)

(defun json-schema-edit-xref-sibling-files ()
  "Return the JSON files in the directory of the current buffer."
  (when buffer-file-name
    (directory-files
     (file-name-directory buffer-file-name)
     t "\\.json\\'")))

;;;; Targets

(defun json-schema-edit-xref--summary (model pointer)
  "Return the summary of the innermost schema object of MODEL holding POINTER.
It is one line of the type and the title or description."
  (let ((node (json-schema-edit-model-node-at-pointer model pointer)))
    (while (and pointer (not (json-schema-edit-model-object-p node)))
      (setq pointer
            (butlast pointer)
            node
            (json-schema-edit-model-node-at-pointer model pointer)))
    (when (json-schema-edit-model-object-p node)
      (let ((type (json-schema-edit-header-type-string node))
            (desc
             (or
              (json-schema-edit-model-get-string node 'title)
              (json-schema-edit-model-get-string node 'description))))
        (string-join (delq nil (list type desc)) " — ")))))

(defun json-schema-edit-xref--describe (model pos)
  "Return a target plist for POS in MODEL.
The plist has :file, :buffer, :pos, :line, :column, :pointer and
:summary."
  (let ((line (json-schema-edit-model-line-column model pos))
        (pointer (json-schema-edit-model-pointer-at model pos)))
    (list :file
          (json-schema-edit-model-file model)
          :buffer (json-schema-edit-model-buffer model)
          :pos pos
          :line (car line)
          :column (cdr line)
          :pointer pointer
          :summary (json-schema-edit-xref--summary model pointer))))

(defun json-schema-edit-xref--target (model ref)
  "Resolve REF from MODEL and return a plist describing its target, or nil.
See `json-schema-edit-xref--describe' for the plist."
  (when-let* ((res (json-schema-edit-model-resolve model ref)))
    (json-schema-edit-xref--describe
     (map-elt res :model)
     (map-elt res :pos))))

(defun json-schema-edit-xref--location (target)
  "Return an xref location for the TARGET plist.
A file location is preferred, since the model may come from a file
that is not visited."
  (if-let* ((file (map-elt target :file)))
      (xref-make-file-location file
                               (map-elt target :line)
                               (map-elt target :column))
    (xref-make-buffer-location
     (map-elt target :buffer)
     (map-elt target :pos))))

;;;; Identifiers

(defun json-schema-edit-xref--identifier-at-point ()
  "Return the identifier at point.
On a `$ref' it is the reference as written; elsewhere it is the URI
fragment of the schema at point."
  (let ((model (json-schema-edit-model-current)))
    (if-let* ((ref (json-schema-edit-model-ref-at model (point))))
        (propertize
         (json-schema-edit-model-ref-value ref)
         'json-schema-edit-ref t)
      (json-schema-edit-model-pointer-fragment
       (json-schema-edit-model-pointer-at model (point))))))

(defun json-schema-edit-xref--definition-names (model)
  "Return fragments of the definitions and anchors in MODEL."
  (let ((root (json-schema-edit-model-root model))
        names)
    (dolist (key '($defs definitions))
      (dolist (def
               (json-schema-edit-model-members
                (json-schema-edit-model-get root key)))
        (push
         (json-schema-edit-model-pointer-fragment
          (list key (json-schema-edit-model-node-key def)))
         names)))
    (dolist (a (json-schema-edit-model-anchors model))
      (push (concat "#" (car a)) names))
    (nreverse names)))

;;;; References

(defun json-schema-edit-xref--target-key (target)
  "Return a key identifying TARGET for comparison."
  (cons
   (or (map-elt target :file) (map-elt target :buffer))
   (map-elt target :pointer)))

(defun json-schema-edit-xref--references-in (model key)
  "Return xref items for the `$ref's of MODEL that resolve to KEY."
  (let (items)
    (dolist (ref (json-schema-edit-model-refs model))
      (when-let* ((target
                   (json-schema-edit-xref--target
                    model
                    (json-schema-edit-model-ref-value ref))))
        (when (equal (json-schema-edit-xref--target-key target) key)
          (let ((pos (json-schema-edit-model-ref-beg ref)))
            (push
             (xref-make
              (json-schema-edit-model-line-string model pos)
              (json-schema-edit-xref--location
               (json-schema-edit-xref--describe model pos)))
             items)))))
    (nreverse items)))

(defun json-schema-edit-xref--references (model target)
  "Return xref items for every `$ref' resolving to TARGET.
MODEL is the model of the current buffer; the files of
`json-schema-edit-reference-files-function' are searched as well."
  (let* ((key (json-schema-edit-xref--target-key target))
         (self (json-schema-edit-model-file model))
         (items (json-schema-edit-xref--references-in model key)))
    (dolist (file (funcall json-schema-edit-reference-files-function))
      (unless (and self (file-equal-p file self))
        (when-let* ((other (json-schema-edit-model-of-file file)))
          (setq items
                (append items
                        (json-schema-edit-xref--references-in other key))))))
    items))

;;;; Backend

;;;###autoload
(defun json-schema-edit-xref-backend ()
  "Return the JSON Schema xref backend."
  'json-schema-edit)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql json-schema-edit)))
  "Return the `$ref' or schema pointer at point."
  (json-schema-edit-xref--identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql json-schema-edit)))
  "Return the definitions and anchors of the buffer."
  (json-schema-edit-xref--definition-names
   (json-schema-edit-model-current)))

(cl-defmethod xref-backend-definitions ((_backend (eql json-schema-edit))
                                        identifier)
  "Return the target of the `$ref' IDENTIFIER."
  (when-let* ((target
               (json-schema-edit-xref--target
                (json-schema-edit-model-current)
                identifier)))
    (list
     (xref-make identifier (json-schema-edit-xref--location target)))))

(cl-defmethod xref-backend-references ((_backend (eql json-schema-edit))
                                       identifier)
  "Return every `$ref' that points to the target of IDENTIFIER."
  (let ((model (json-schema-edit-model-current)))
    (when-let* ((target (json-schema-edit-xref--target model identifier)))
      (json-schema-edit-xref--references model target))))

;;;; Eldoc

(defun json-schema-edit-xref-eldoc-function (callback &rest _)
  "Report a summary of the `$ref' target at point to CALLBACK."
  (when-let* ((model (json-schema-edit-model-current))
              (ref (json-schema-edit-model-ref-at model (point)))
              (value (json-schema-edit-model-ref-value ref)))
    (let ((target (json-schema-edit-xref--target model value)))
      (funcall callback
               (if target
                   (or (map-elt target :summary) "")
                 (propertize "unresolved" 'face 'warning))
               :thing value
               :face 'font-lock-constant-face))))

(provide 'json-schema-edit-xref)
;;; json-schema-edit-xref.el ends here
