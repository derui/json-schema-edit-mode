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
  :type '(choice
          (const :tag "Current file and other schema files" directory)
          (const :tag "Current file only" file)
          (const :tag "Disabled" nil))
  :group 'json-schema-edit)

;;;; Checks

(defun json-schema-edit-lint--diag (node type format &rest args)
  "Return a diagnostic of TYPE on NODE with message FORMAT and ARGS."
  (list
   (json-schema-edit-model-node-beg node)
   (json-schema-edit-model-node-end node)
   type
   (apply #'format-message format args)))

(defun json-schema-edit-lint--key-diag (member type format &rest args)
  "Return a diagnostic of TYPE on the key of MEMBER with message FORMAT and ARGS."
  (list
   (json-schema-edit-model-node-key-beg member)
   (json-schema-edit-model-node-key-end member)
   type
   (apply #'format-message format args)))

(defun json-schema-edit-lint--check-required (schema)
  "Return diagnostics for `required' names missing from `properties' of SCHEMA."
  (let ((props (json-schema-edit-model-get schema 'properties))
        (required (json-schema-edit-model-get schema 'required)))
    (when (json-schema-edit-model-object-p props)
      (delq nil
            (mapcar
             (lambda (n)
               (let ((name (json-schema-edit-model-string-value n)))
                 (when (and name
                            (not (json-schema-edit-model-get props name)))
                   (json-schema-edit-lint--diag
                    n :error "`%s' is required but not in properties" name))))
             (json-schema-edit-model-elements required))))))

(defun json-schema-edit-lint--check-type-keywords (schema)
  "Return diagnostics for keywords of SCHEMA that do not apply to its type."
  (when-let* ((types (json-schema-edit-model-declared-types schema)))
    (delq nil
          (mapcar
           (lambda (member)
             (let ((key (json-schema-edit-model-node-key member)))
               (unless (json-schema-edit-model-keyword-applies-p key types)
                 (json-schema-edit-lint--key-diag
                  member :warning
                  "`%s' has no effect on type %s" key
                  (string-join types " | ")))))
           (json-schema-edit-model-members schema)))))

(defun json-schema-edit-lint--check-refs (model)
  "Return diagnostics for the `$ref's of MODEL that cannot be resolved."
  (delq nil
        (mapcar
         (lambda (ref)
           (let ((value (json-schema-edit-model-ref-value ref)))
             (unless (or
                      (null value)
                      (json-schema-edit-model-remote-p
                       (car (json-schema-edit-model-split-ref value)))
                      (json-schema-edit-model-resolve model value))
               (list
                (json-schema-edit-model-ref-beg ref)
                (json-schema-edit-model-ref-end ref)
                :error (format-message "Cannot resolve `%s'" value)))))
         (json-schema-edit-model-refs model))))

;;;; Combinators

(defun json-schema-edit-lint--const-of (target prop)
  "Return the `const' node of the property PROP in the schema TARGET."
  (json-schema-edit-model-get
   (json-schema-edit-model-get
    (json-schema-edit-model-get target 'properties)
    prop)
   'const))

(defun json-schema-edit-lint--discriminator-candidate (node branches)
  "Return the property of NODE meant to discriminate BRANCHES, or nil.
It is the OpenAPI `discriminator.propertyName', or a property fixed by
`const' in at least two branches and half of them."
  (or
   (json-schema-edit-model-get-string
    (json-schema-edit-model-get node 'discriminator)
    'propertyName)
   (let ((counts nil))
     (dolist (b branches)
       (dolist (member
                (json-schema-edit-model-members
                 (json-schema-edit-model-get (nth 3 b) 'properties)))
         (when (json-schema-edit-model-get member 'const)
           (cl-incf
            (alist-get
             (json-schema-edit-model-node-key member)
             counts 0 nil #'equal)))))
     (car
      (seq-find
       (lambda (c)
         (and
          (>= (cdr c) 2)
          (>= (* 2 (cdr c)) (length branches))))
       (nreverse counts))))))

(defun json-schema-edit-lint--check-discriminated (model node pointer comb)
  "Return diagnostics for the discriminating property of COMB in NODE at POINTER.
NODE is a schema of MODEL."
  (let* ((branches
          (json-schema-edit-outline-branches model pointer comb))
         (prop
          (json-schema-edit-lint--discriminator-candidate node branches))
         seen result)
    (when prop
      (dolist (b branches)
        (let ((c (json-schema-edit-lint--const-of (nth 3 b) prop)))
          (cond
           ((null c)
            (push
             (json-schema-edit-lint--diag
              (cadr b)
              :warning "Branch does not fix the discriminating property `%s'" prop)
             result))
           ((member (json-schema-edit-model-node-text c) seen)
            (push
             (json-schema-edit-lint--diag
              c :error "Duplicate discriminating value %s"
              (json-schema-edit-model-node-text c))
             result))
           (t (push (json-schema-edit-model-node-text c) seen)))))
      (dolist (member
               (json-schema-edit-model-members
                (json-schema-edit-model-get
                 (json-schema-edit-model-get node 'discriminator)
                 'mapping)))
        (let* ((value (json-schema-edit-model-node-key member))
               (ref (json-schema-edit-model-string-value member))
               (res
                (and ref (json-schema-edit-model-resolve model ref)))
               (c
                (and res
                     (not (map-elt res :file))
                     (json-schema-edit-lint--const-of
                      (map-elt res :node)
                      prop))))
          (when (and c
                     (not (equal
                           (json-schema-edit-model-string-value c)
                           value)))
            (push
             (json-schema-edit-lint--diag
              member :warning
              "Mapping `%s' points to a branch whose %s is %s"
              value prop
              (json-schema-edit-model-node-text c))
             result)))))
    (nreverse result)))

(defun json-schema-edit-lint--check-overlap (model pointer)
  "Return diagnostics for overlapping oneOf branches of MODEL at POINTER.
This is a heuristic: identical branches, or branches naming only the
same type."
  (let ((seen nil)
        result)
    (seq-do-indexed
     (lambda (b i)
       (let* ((branch (cadr b))
              (key
               (if (seq-every-p
                    (lambda (m)
                      (memq (json-schema-edit-model-node-key m)
                            '(type title description)))
                    (json-schema-edit-model-members branch))
                   (format "type:%s"
                           (json-schema-edit-model-declared-types branch))
                 (replace-regexp-in-string "[ \t\n]+" ""
                                           (json-schema-edit-model-node-text branch))))
              (other (assoc key seen)))
         (if other
             (push
              (json-schema-edit-lint--diag
               branch :warning
               "Branches #%d and #%d accept the same values, which oneOf rejects"
               (1+ (cdr other))
               (1+ i))
              result)
           (push (cons key i) seen))))
     (json-schema-edit-outline-branches model pointer 'oneOf))
    (nreverse result)))

(defun json-schema-edit-lint--check-all-of (model node pointer)
  "Return diagnostics for allOf of NODE at POINTER, a schema of MODEL."
  (let* ((branches
          (json-schema-edit-outline-branches model pointer 'allOf))
         (all-of (json-schema-edit-model-get node 'allOf))
         (widen
          (lambda (types)
            (if (member "number" types) (cons "integer" types) types)))
         (possible :any)
         result)
    (dolist (types
             (delq nil
                   (cons
                    (json-schema-edit-model-declared-types node)
                    (mapcar
                     (lambda (b)
                       (json-schema-edit-model-declared-types (nth 3 b)))
                     branches))))
      (setq possible
            (if (eq possible :any)
                (funcall widen types)
              (seq-intersection possible (funcall widen types)))))
    (when (null possible)
      (push
       (json-schema-edit-lint--key-diag
        all-of :error "The types of the allOf branches leave no possible value")
       result))
    (let ((names-of
           (lambda (n)
             (mapcar #'json-schema-edit-model-node-key
                     (json-schema-edit-model-members
                      (json-schema-edit-model-get n 'properties))))))
      (dolist (b branches)
        (let ((additional
               (json-schema-edit-model-get
                (nth 3 b)
                'additionalProperties)))
          (when (eq
                 (json-schema-edit-model-json-type additional)
                 'false)
            (let* ((own (funcall names-of (nth 3 b)))
                   (others
                    (seq-uniq
                     (seq-difference
                      (append
                       (funcall names-of node)
                       (mapcan
                        (lambda (o)
                          (unless (eq o b)
                            (funcall names-of (nth 3 o))))
                        branches))
                      own))))
              (when others
                (push
                 (json-schema-edit-lint--diag
                  additional :warning
                  "additionalProperties: false rejects %s, defined outside this branch"
                  (string-join others ", "))
                 result)))))))
    (nreverse result)))

(defun json-schema-edit-lint--check-combinators (model schema)
  "Return diagnostics for the combinators of SCHEMA, a schema of MODEL."
  (let ((pointer (json-schema-edit-model-node-pointer schema)))
    (append
     (mapcan
      (lambda (comb)
        (when (json-schema-edit-model-array-p
               (json-schema-edit-model-get schema comb))
          (append
           (json-schema-edit-lint--check-discriminated model schema pointer comb)
           (when (eq comb 'oneOf)
             (json-schema-edit-lint--check-overlap model pointer)))))
      '(oneOf anyOf))
     (when (json-schema-edit-model-array-p
            (json-schema-edit-model-get schema 'allOf))
       (json-schema-edit-lint--check-all-of model schema pointer)))))

;;;; Unused definitions

(defun json-schema-edit-lint--used-pointers (model)
  "Return the pointers in MODEL that some `$ref' points to."
  (let ((self (json-schema-edit-model-file model))
        result)
    (dolist (ref (json-schema-edit-model-refs model))
      (when-let* ((res
                   (json-schema-edit-model-resolve
                    model
                    (json-schema-edit-model-ref-value ref))))
        (unless (and
                 (map-elt res :file)
                 self
                 (not (file-equal-p (map-elt res :file) self)))
          (push (map-elt res :pointer) result))))
    (when (and self
               (eq json-schema-edit-lint-unused-definitions 'directory))
      (dolist (file (funcall json-schema-edit-reference-files-function))
        (unless (file-equal-p file self)
          (when-let* ((other (json-schema-edit-model-of-file file)))
            (dolist (ref (json-schema-edit-model-refs other))
              (pcase-let ((`(,resource . ,fragment)
                           (json-schema-edit-model-split-ref
                            (or (json-schema-edit-model-ref-value ref) ""))))
                (when (and
                       (not (string-empty-p resource))
                       (not (json-schema-edit-model-remote-p resource))
                       (file-equal-p
                        (json-schema-edit-model-resource-file other resource)
                        self))
                  (when-let* ((node
                               (json-schema-edit-model-resolve-fragment model fragment)))
                    (push
                     (json-schema-edit-model-node-pointer node)
                     result)))))))))
    result))

(defun json-schema-edit-lint--check-unused (model)
  "Return diagnostics for the definitions of MODEL that no `$ref' uses."
  (when json-schema-edit-lint-unused-definitions
    (let ((used (json-schema-edit-lint--used-pointers model))
          (root (json-schema-edit-model-root model))
          result)
      (dolist (keyword '($defs definitions))
        (dolist (def
                 (json-schema-edit-model-members
                  (json-schema-edit-model-get root keyword)))
          (let* ((name (json-schema-edit-model-node-key def))
                 (prefix (list keyword name)))
            (unless (or
                     (seq-some
                      (lambda (p) (equal (seq-take p 2) prefix))
                      used)
                     ;; Reachable by `$anchor' or `$id' from outside.
                     (json-schema-edit-model-get def '$anchor)
                     (json-schema-edit-model-get def '$id))
              (push
               (json-schema-edit-lint--key-diag
                def :note "Definition `%s' is not used" name)
               result)))))
      (nreverse result))))

;;;; Entry points

(defun json-schema-edit-lint (model)
  "Return the diagnostics of MODEL.
Each is a list (BEG END TYPE MESSAGE) where TYPE is a flymake type."
  (append
   (json-schema-edit-lint--check-refs model)
   (json-schema-edit-lint--check-unused model)
   (mapcan
    (lambda (schema)
      (append
       (json-schema-edit-lint--check-required schema)
       (json-schema-edit-lint--check-type-keywords schema)
       (json-schema-edit-lint--check-combinators model schema)))
    (json-schema-edit-model-schemas model))))

(defun json-schema-edit-lint-flymake (report-fn &rest _)
  "Flymake backend reporting `json-schema-edit-lint' to REPORT-FN."
  (funcall report-fn
           (mapcar
            (lambda (d)
              (flymake-make-diagnostic
               (current-buffer)
               (nth 0 d)
               (nth 1 d)
               (nth 2 d)
               (nth 3 d)))
            (json-schema-edit-lint (json-schema-edit-model-current)))))

(provide 'json-schema-edit-lint)
;;; json-schema-edit-lint.el ends here
