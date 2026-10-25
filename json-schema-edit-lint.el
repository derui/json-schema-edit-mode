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
;; - oneOf / anyOf: duplicated or missing discriminating values, a
;;   mapping that disagrees with the branches, oneOf branches that
;;   accept the same values
;; - allOf: types no value can satisfy, and `additionalProperties: false'
;;   rejecting properties added by the other branches

;;; Code:

(require 'flymake)
(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-xref)
(require 'json-schema-edit-outline)

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
  (let ((props (json-schema-edit-model-get schema 'properties))
        (required (json-schema-edit-model-get schema 'required)))
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

;;;; Combinators

(defun json-schema-edit-lint--const-of (target prop)
  "Return the `const' node of the property PROP in the schema TARGET."
  (json-schema-edit-model-get
   (json-schema-edit-model-get (json-schema-edit-model-get target 'properties) prop)
   'const))

(defun json-schema-edit-lint--discriminator-candidate (node branches)
  "Return the property of NODE meant to discriminate BRANCHES, or nil.
It is the OpenAPI `discriminator.propertyName', or a property fixed by
`const' in at least two branches and half of them."
  (or (json-schema-edit-model-get-string
       (json-schema-edit-model-get node 'discriminator) 'propertyName)
      (let ((counts nil))
        (dolist (b branches)
          (dolist (pair (json-schema-edit-model-object-pairs
                         (json-schema-edit-model-get (nth 3 b) 'properties)))
            (when (json-schema-edit-model-get (json-schema-edit-model-pair-value pair) 'const)
              (cl-incf (alist-get (json-schema-edit-model-pair-key pair) counts 0 nil #'equal)))))
        (car (seq-find (lambda (c) (and (>= (cdr c) 2) (>= (* 2 (cdr c)) (length branches))))
                       (nreverse counts))))))

(defun json-schema-edit-lint--check-discriminated (node pointer comb)
  "Return diagnostics for the discriminating property of COMB in NODE at POINTER."
  (let* ((branches (json-schema-edit-outline-branches node pointer comb))
         (prop (json-schema-edit-lint--discriminator-candidate node branches))
         seen result)
    (when prop
      (dolist (b branches)
        (let ((c (json-schema-edit-lint--const-of (nth 3 b) prop)))
          (cond
           ((null c)
            (push (json-schema-edit-lint--diag
                   (cadr b) :warning "Branch does not fix the discriminating property `%s'" prop)
                  result))
           ((member (treesit-node-text c t) seen)
            (push (json-schema-edit-lint--diag
                   c :error "Duplicate discriminating value %s" (treesit-node-text c t))
                  result))
           (t (push (treesit-node-text c t) seen)))))
      (dolist (pair (json-schema-edit-model-object-pairs
                     (json-schema-edit-model-get
                      (json-schema-edit-model-get node 'discriminator) 'mapping)))
        (let* ((value (json-schema-edit-model-pair-key pair))
               (ref (json-schema-edit-model-string-value (json-schema-edit-model-pair-value pair)))
               (res (and ref (json-schema-edit-model-resolve ref)))
               (c (and res (not (map-elt res :file))
                       (json-schema-edit-lint--const-of
                        (json-schema-edit-model-node-at-pointer (map-elt res :pointer)) prop))))
          (when (and c (not (equal (json-schema-edit-model-string-value c) value)))
            (push (json-schema-edit-lint--diag
                   (json-schema-edit-model-pair-value pair) :warning
                   "Mapping `%s' points to a branch whose %s is %s"
                   value prop (treesit-node-text c t))
                  result)))))
    (nreverse result)))

(defun json-schema-edit-lint--check-overlap (node pointer)
  "Return diagnostics for oneOf branches of NODE at POINTER that overlap.
This is a heuristic: identical branches, or branches naming only the
same type."
  (let ((seen nil) result)
    (seq-do-indexed
     (lambda (b i)
       (let* ((branch (cadr b))
              (key (if (seq-every-p (lambda (p) (memq (json-schema-edit-model-keyword
                                                       (json-schema-edit-model-pair-key p))
                                                      '(type title description)))
                                    (json-schema-edit-model-object-pairs branch))
                       (format "type:%s" (json-schema-edit-model-declared-types branch))
                     (replace-regexp-in-string "[ \t\n]+" "" (treesit-node-text branch t))))
              (other (assoc key seen)))
         (if other
             (push (json-schema-edit-lint--diag
                    branch :warning
                    "Branches #%d and #%d accept the same values, which oneOf rejects"
                    (1+ (cdr other)) (1+ i))
                   result)
           (push (cons key i) seen))))
     (json-schema-edit-outline-branches node pointer 'oneOf))
    (nreverse result)))

(defun json-schema-edit-lint--check-all-of (node pointer)
  "Return diagnostics for allOf of NODE at POINTER."
  (let* ((branches (json-schema-edit-outline-branches node pointer 'allOf))
         (all-of (json-schema-edit-model-get node 'allOf))
         (widen (lambda (types) (if (member "number" types) (cons "integer" types) types)))
         (possible :any)
         result)
    (dolist (types (delq nil (cons (json-schema-edit-model-declared-types node)
                                   (mapcar (lambda (b) (json-schema-edit-model-declared-types
                                                        (nth 3 b)))
                                           branches))))
      (setq possible (if (eq possible :any)
                         (funcall widen types)
                       (seq-intersection possible (funcall widen types)))))
    (when (null possible)
      (push (json-schema-edit-lint--diag
             (treesit-node-child-by-field-name (treesit-node-parent all-of) "key")
             :error "The types of the allOf branches leave no possible value")
            result))
    (let ((names-of (lambda (n) (mapcar #'json-schema-edit-model-pair-key
                                        (json-schema-edit-model-object-pairs
                                         (json-schema-edit-model-get n 'properties))))))
      (dolist (b branches)
        (let ((additional (json-schema-edit-model-get (nth 3 b) 'additionalProperties)))
          (when (equal (treesit-node-type additional) "false")
            (let* ((own (funcall names-of (nth 3 b)))
                   (others (seq-uniq
                            (seq-difference
                             (append (funcall names-of node)
                                     (mapcan (lambda (o) (unless (eq o b)
                                                           (funcall names-of (nth 3 o))))
                                             branches))
                             own))))
              (when others
                (push (json-schema-edit-lint--diag
                       additional :warning
                       "additionalProperties: false rejects %s, defined outside this branch"
                       (string-join others ", "))
                      result)))))))
    (nreverse result)))

(defun json-schema-edit-lint--check-combinators (pointer schema)
  "Return diagnostics for the combinators of SCHEMA at POINTER."
  (append
   (mapcan (lambda (comb)
             (when (equal (treesit-node-type (json-schema-edit-model-get schema comb)) "array")
               (append (json-schema-edit-lint--check-discriminated schema pointer comb)
                       (when (eq comb 'oneOf)
                         (json-schema-edit-lint--check-overlap schema pointer)))))
           '(oneOf anyOf))
   (when (equal (treesit-node-type (json-schema-edit-model-get schema 'allOf)) "array")
     (json-schema-edit-lint--check-all-of schema pointer))))

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
      (dolist (keyword '($defs definitions))
        (dolist (pair (json-schema-edit-model-object-pairs
                       (json-schema-edit-model-get root keyword)))
          (let* ((name (json-schema-edit-model-pair-key pair))
                 (prefix (json-schema-edit-model-normalize-segments (list keyword name)))
                 (value (json-schema-edit-model-pair-value pair)))
            (unless (or (seq-some (lambda (p) (equal (seq-take p 2) prefix)) used)
                        ;; Reachable by `$anchor' or `$id' from outside.
                        (json-schema-edit-model-get value '$anchor)
                        (json-schema-edit-model-get value '$id))
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
                       (json-schema-edit-lint--check-type-keywords (cdr entry))
                       (json-schema-edit-lint--check-combinators (car entry) (cdr entry))))
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
