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
    (directory-files (file-name-directory buffer-file-name) t "\\.json\\'")))

;;;; Targets

(defun json-schema-edit-xref--target (ref)
  "Resolve REF and return a plist describing its target, or nil.
The plist has :file (absolute, or nil for a buffer without a file),
:buffer (when the target is in the current buffer), :pos, :line, :column,
:pointer and :summary."
  (when-let* ((res (json-schema-edit-model-resolve ref)))
    (let ((file (map-elt res :file))
          (pos (map-elt res :pos)))
      (if file
          (json-schema-edit-model-with-file file
            (json-schema-edit-xref--describe pos file nil))
        (json-schema-edit-xref--describe pos buffer-file-name (current-buffer))))))

(defun json-schema-edit-xref--describe (pos file buffer)
  "Return a target plist for POS in the current buffer.
FILE and BUFFER are recorded as they are."
  (save-excursion
    (goto-char pos)
    (list :file file
          :buffer buffer
          :pos pos
          :line (line-number-at-pos)
          :column (current-column)
          :pointer (json-schema-edit-model-pointer-at pos)
          :summary (when-let* ((node (treesit-node-at pos 'json))
                               (obj (treesit-parent-until
                                     node (lambda (n) (equal (treesit-node-type n) "object"))
                                     t)))
                     (json-schema-edit-model-summary obj)))))

(defun json-schema-edit-xref--location (target)
  "Return an xref location for the TARGET plist.
A file location is preferred, since the buffer may be a temporary one."
  (if-let* ((file (map-elt target :file)))
      (xref-make-file-location file
                               (map-elt target :line)
                               (map-elt target :column))
    (xref-make-buffer-location (map-elt target :buffer)
                               (map-elt target :pos))))

(defun json-schema-edit-xref--line-summary (pos)
  "Return the trimmed text of the line at POS."
  (save-excursion
    (goto-char pos)
    (string-trim (buffer-substring (line-beginning-position) (line-end-position)))))

;;;; Identifiers

(defun json-schema-edit-xref--identifier-at-point ()
  "Return the identifier at point.
On a `$ref' it is the reference as written; elsewhere it is the URI
fragment of the schema at point."
  (if-let* ((ref (json-schema-edit-model-ref-at)))
      (propertize (json-schema-edit-model-ref-value ref) 'json-schema-edit-ref t)
    (json-schema-edit-model-pointer-fragment (json-schema-edit-model-pointer-at))))

(defun json-schema-edit-xref--definition-names ()
  "Return fragments of the definitions and anchors in the current buffer."
  (let ((root (json-schema-edit-model-root)) names)
    (dolist (key '("$defs" "definitions"))
      (dolist (pair (json-schema-edit-model-object-pairs
                     (json-schema-edit-model-get root key)))
        (push (json-schema-edit-model-pointer-fragment
               (list key (json-schema-edit-model-pair-key pair)))
              names)))
    (dolist (a (map-elt (json-schema-edit-model-index) :anchors))
      (push (concat "#" (car a)) names))
    (nreverse names)))

;;;; References

(defun json-schema-edit-xref--target-key (target)
  "Return a key identifying TARGET for comparison."
  (cons (or (map-elt target :file) (map-elt target :buffer))
        (json-schema-edit-model-normalize-segments (map-elt target :pointer))))

(defun json-schema-edit-xref--references-in-buffer (key)
  "Return xref items for the `$ref's of the current buffer that resolve to KEY."
  (let (items)
    (dolist (ref (json-schema-edit-model-refs))
      (when-let* ((target (json-schema-edit-xref--target
                           (json-schema-edit-model-ref-value ref))))
        (when (equal (json-schema-edit-xref--target-key target) key)
          (let ((pos (json-schema-edit-model-ref-beg ref)))
            (push (xref-make
                   (json-schema-edit-xref--line-summary pos)
                   (json-schema-edit-xref--location
                    (json-schema-edit-xref--describe
                     pos buffer-file-name (current-buffer))))
                  items)))))
    (nreverse items)))

(defun json-schema-edit-xref--references (target)
  "Return xref items for every `$ref' resolving to TARGET."
  (let* ((key (json-schema-edit-xref--target-key target))
         (self buffer-file-name)
         (items (save-restriction
                  (widen)
                  (json-schema-edit-xref--references-in-buffer key))))
    (dolist (file (funcall json-schema-edit-reference-files-function))
      (unless (and self (file-equal-p file self))
        (when (file-readable-p file)
          (setq items
                (append items
                        (json-schema-edit-model-with-file file
                          (let ((buffer-file-name (or buffer-file-name file)))
                            (json-schema-edit-xref--references-in-buffer key))))))))
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
  (json-schema-edit-xref--definition-names))

(cl-defmethod xref-backend-definitions ((_backend (eql json-schema-edit)) identifier)
  "Return the target of the `$ref' IDENTIFIER."
  (when-let* ((target (json-schema-edit-xref--target identifier)))
    (list (xref-make identifier (json-schema-edit-xref--location target)))))

(cl-defmethod xref-backend-references ((_backend (eql json-schema-edit)) identifier)
  "Return every `$ref' that points to the target of IDENTIFIER."
  (when-let* ((target (json-schema-edit-xref--target identifier)))
    (json-schema-edit-xref--references target)))

;;;; Eldoc

(defun json-schema-edit-xref-eldoc-function (callback &rest _)
  "Report a summary of the `$ref' target at point to CALLBACK."
  (when-let* ((ref (json-schema-edit-model-ref-at))
              (value (json-schema-edit-model-ref-value ref)))
    (let ((target (json-schema-edit-xref--target value)))
      (funcall callback
               (if target
                   (or (map-elt target :summary) "")
                 (propertize "unresolved" 'face 'warning))
               :thing value
               :face 'font-lock-constant-face))))

(provide 'json-schema-edit-xref)
;;; json-schema-edit-xref.el ends here
