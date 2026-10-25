;;; json-schema-edit-lint.el --- Schema-specific lint via flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Catches mistakes in how a schema is written, which metaschema
;; validation (left to LSP) does not report:
;;
;; - names in `required' that are missing from `properties'
;; - definitions that no `$ref' uses
;; - `$ref's that cannot be resolved
;; - keywords that do not apply to the declared `type'

;;; Code:

(require 'flymake)
(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-xref)

(defcustom json-schema-edit-lint-unused-definitions 'directory
  "Where `$ref's are looked for when checking for unused definitions.
`directory' also counts references from the files returned by
`json-schema-edit-reference-files-function'; `file' only counts the
current buffer; nil disables the check."
  :type '(choice (const :tag "Current file and other schema files" directory)
                 (const :tag "Current file only" file)
                 (const :tag "Disabled" nil))
  :group 'json-schema-edit)

;;;; Checks

(defun json-schema-edit-lint--diag (node type format &rest args)
  "Return a diagnostic of TYPE on NODE with message FORMAT and ARGS."
  (list (treesit-node-start node) (treesit-node-end node) type
        (apply #'format-message format args)))

(defun json-schema-edit-lint--schemas ()
  "Return (POINTER . NODE) for every schema object in the buffer."
  (let (result)
    (dolist (c (treesit-query-capture (json-schema-edit-model--parser)
                                      '((object) @o)))
      (let* ((node (cdr c))
             (pointer (json-schema-edit-model-pointer-at (treesit-node-start node))))
        (when (json-schema-edit-model-schema-position-p pointer)
          (push (cons pointer node) result))))
    (nreverse result)))

(defun json-schema-edit-lint--check-required (schema)
  "Return diagnostics for `required' names missing from `properties' of SCHEMA."
  (let ((props (json-schema-edit-model-get schema "properties"))
        (required (json-schema-edit-model-get schema "required")))
    (when (and (equal (treesit-node-type props) "object")
               (equal (treesit-node-type required) "array"))
      (delq nil
            (mapcar (lambda (n)
                      (let ((name (json-schema-edit-model-string-value n)))
                        (when (and name (not (json-schema-edit-model-get props name)))
                          (json-schema-edit-lint--diag
                           n :error "`%s' is required but not in properties" name))))
                    (json-schema-edit-model--value-children required))))))

(defun json-schema-edit-lint--check-type-keywords (schema)
  "Return diagnostics for keywords of SCHEMA that do not apply to its type."
  (when-let* ((types (json-schema-edit-model-declared-types schema)))
    (delq nil
          (mapcar (lambda (pair)
                    (let ((key (json-schema-edit-model-pair-key pair)))
                      (unless (json-schema-edit-model-keyword-applies-p key types)
                        (json-schema-edit-lint--diag
                         (treesit-node-child-by-field-name pair "key") :warning
                         "`%s' has no effect on type %s" key (string-join types " | ")))))
                  (json-schema-edit-model-object-pairs schema)))))

(defun json-schema-edit-lint--check-refs ()
  "Return diagnostics for `$ref's that cannot be resolved."
  (delq nil
        (mapcar (lambda (ref)
                  (let ((value (json-schema-edit-model-ref-value ref)))
                    (unless (or (null value)
                                (json-schema-edit-model-remote-p
                                 (car (json-schema-edit-model-split-ref value)))
                                (json-schema-edit-model-resolve value))
                      (list (json-schema-edit-model-ref-beg ref)
                            (json-schema-edit-model-ref-end ref)
                            :error
                            (format-message "Cannot resolve `%s'" value)))))
                (json-schema-edit-model-refs))))

;;;; Unused definitions

(defvar json-schema-edit-lint--external-cache (make-hash-table :test #'equal)
  "File name to (MTIME . REFS) of the `$ref' strings in that file.")

(defun json-schema-edit-lint--file-refs (file)
  "Return the `$ref' strings in FILE, cached by modification time."
  (let ((buf (find-buffer-visiting file))
        (mtime (file-attribute-modification-time (file-attributes file))))
    (if (and buf (buffer-modified-p buf))
        (json-schema-edit-model-with-file file
          (mapcar #'json-schema-edit-model-ref-value (json-schema-edit-model-refs)))
      (let ((cached (gethash file json-schema-edit-lint--external-cache)))
        (if (equal (car cached) mtime)
            (cdr cached)
          (let ((refs (json-schema-edit-model-with-file file
                        (mapcar #'json-schema-edit-model-ref-value
                                (json-schema-edit-model-refs)))))
            (puthash file (cons mtime refs) json-schema-edit-lint--external-cache)
            refs))))))

(defun json-schema-edit-lint--used-pointers ()
  "Return the pointers in this buffer that some `$ref' points to."
  (let ((self buffer-file-name) result)
    (dolist (ref (json-schema-edit-model-refs))
      (when-let* ((res (json-schema-edit-model-resolve
                        (json-schema-edit-model-ref-value ref))))
        (unless (and (map-elt res :file) self
                     (not (file-equal-p (map-elt res :file) self)))
          (push (map-elt res :pointer) result))))
    (when (and self (eq json-schema-edit-lint-unused-definitions 'directory))
      (dolist (file (funcall json-schema-edit-reference-files-function))
        (unless (or (file-equal-p file self) (not (file-readable-p file)))
          (dolist (value (json-schema-edit-lint--file-refs file))
            (pcase-let ((`(,resource . ,fragment)
                         (json-schema-edit-model-split-ref (or value ""))))
              (when (and (not (string-empty-p resource))
                         (not (json-schema-edit-model-remote-p resource))
                         (file-equal-p (expand-file-name
                                        (url-unhex-string resource)
                                        (file-name-directory file))
                                       self))
                (when-let* ((pos (json-schema-edit-model--resolve-fragment fragment)))
                  (push (json-schema-edit-model-pointer-at pos) result))))))))
    (mapcar #'json-schema-edit-model-normalize-segments result)))

(defun json-schema-edit-lint--check-unused ()
  "Return diagnostics for definitions that no `$ref' uses."
  (when json-schema-edit-lint-unused-definitions
    (let ((used (json-schema-edit-lint--used-pointers))
          (root (json-schema-edit-model-root))
          result)
      (dolist (keyword '("$defs" "definitions"))
        (dolist (pair (json-schema-edit-model-object-pairs
                       (json-schema-edit-model-get root keyword)))
          (let* ((name (json-schema-edit-model-pair-key pair))
                 (prefix (list keyword name))
                 (value (json-schema-edit-model-pair-value pair)))
            (unless (or (seq-some (lambda (p) (equal (seq-take p 2) prefix)) used)
                        ;; Reachable by `$anchor' or `$id' from outside.
                        (json-schema-edit-model-get value "$anchor")
                        (json-schema-edit-model-get value "$id"))
              (push (json-schema-edit-lint--diag
                     (treesit-node-child-by-field-name pair "key") :note
                     "Definition `%s' is not used" name)
                    result)))))
      (nreverse result))))

;;;; Entry points

(defun json-schema-edit-lint ()
  "Return the diagnostics of the current buffer.
Each is a list (BEG END TYPE MESSAGE) where TYPE is a flymake type."
  (save-restriction
    (widen)
    (json-schema-edit-model-with-cache
      (append
     (json-schema-edit-lint--check-refs)
     (json-schema-edit-lint--check-unused)
     (mapcan (lambda (entry)
               (append (json-schema-edit-lint--check-required (cdr entry))
                       (json-schema-edit-lint--check-type-keywords (cdr entry))))
             (json-schema-edit-lint--schemas))))))

(defun json-schema-edit-lint-flymake (report-fn &rest _)
  "Flymake backend reporting `json-schema-edit-lint' to REPORT-FN."
  (funcall report-fn
           (mapcar (lambda (d)
                     (flymake-make-diagnostic (current-buffer)
                                              (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)))
                   (json-schema-edit-lint))))

(provide 'json-schema-edit-lint)
;;; json-schema-edit-lint.el ends here
