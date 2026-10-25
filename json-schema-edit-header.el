;;; json-schema-edit-header.el --- Header line of JSON Schema buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The breadcrumb shown in the header line by `json-schema-edit-mode':
;; where in the data the schema at point applies, such as
;; "User › address › zip : string *", or the JSON Pointer of the schema.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'json-schema-edit-model)

(defcustom json-schema-edit-breadcrumb-style 'instance
  "How the header line shows the location at point.
`instance' shows the path of the data the schema describes, such as
\"User › address › zip\"; `schema' shows the JSON Pointer in the
schema; nil disables the header line."
  :type '(choice
          (const :tag "Instance path" instance)
          (const :tag "Schema pointer" schema)
          (const :tag "None" nil))
  :group 'json-schema-edit)

(defcustom json-schema-edit-breadcrumb-separator " › "
  "Separator between breadcrumb segments."
  :type 'string
  :group 'json-schema-edit)

;;;; Representation

(defun json-schema-edit-header-type-string (node)
  "Return a short type description of the schema NODE, or nil."
  (when (json-schema-edit-model-object-p node)
    (let ((type (json-schema-edit-model-get node 'type)))
      (pcase (json-schema-edit-model-json-type type)
        ('nil
         (when-let* ((ref (json-schema-edit-model-get-string node '$ref)))
           (concat "→ " ref)))
        ('string
         (json-schema-edit-model-string-value type))
        ('array
         (mapconcat #'json-schema-edit-model-string-value
                    (json-schema-edit-model-elements type)
                    " | "))))))

;;;; Breadcrumb

(defun json-schema-edit-header--branch-name (node index)
  "Return a name for the combinator branch NODE at INDEX."
  (or
   (json-schema-edit-model-get-string node 'title)
   (when-let* ((ref (json-schema-edit-model-get-string node '$ref)))
     (car (last (split-string ref "/"))))
   (format "#%d" (1+ index))))

(defun json-schema-edit-header-instance-path (model segments)
  "Convert schema pointer SEGMENTS of MODEL to a list of instance path labels.
The first label names the root schema, or the definition when SEGMENTS
point into `$defs' / `definitions'."
  (let ((segments (json-schema-edit-model-canonical-pointer segments))
        (labels (list (json-schema-edit-model-root-name model)))
        (node (json-schema-edit-model-root model)))
    (while segments
      (let ((seg (pop segments)))
        (pcase seg
          ((or '$defs 'definitions)
           (when segments
             (let ((name (pop segments)))
               (setq labels
                     (list name)
                     node
                     (json-schema-edit-model-lookup node
                                                    (list seg name))))
             (setq seg nil)))
          ((and
            (or 'properties 'patternProperties 'dependentSchemas)
            keyword)
           (when segments
             (let ((name (pop segments)))
               (push
                (if (eq keyword 'patternProperties)
                    (format "/%s/" name)
                  name)
                labels)
               (setq node
                     (json-schema-edit-model-lookup node
                                                    (list seg name))))
             (setq seg nil)))
          ((or 'items 'additionalItems 'contains)
           (push "[]" labels))
          ('prefixItems
           (when segments
             (let ((idx (pop segments)))
               (push (format "[%s]" idx) labels)
               (setq node
                     (json-schema-edit-model-lookup node
                                                    (list seg idx))))
             (setq seg nil)))
          ('additionalProperties
           (push "{*}" labels))
          ((pred
            (lambda (k) (memq k json-schema-edit-model--combinators)))
           (when segments
             (let* ((idx (pop segments))
                    (branch
                     (json-schema-edit-model-lookup node
                                                    (list seg idx))))
               (push
                (format "‹%s: %s›" seg
                        (json-schema-edit-header--branch-name branch idx))
                labels)
               (setq node branch))
             (setq seg nil))))
        (when seg
          (setq node (json-schema-edit-model-lookup node (list seg))))))
    (nreverse labels)))

(defun json-schema-edit-header--required-p (model segments)
  "Return non-nil if the property of MODEL at SEGMENTS is listed in `required'."
  (let ((n (length segments)))
    (when (and
           (>= n 2)
           (eq (nth (- n 2) segments) 'properties))
      (let* ((owner
              (json-schema-edit-model-node-at-pointer model
                                                      (butlast segments 2)))
             (required (json-schema-edit-model-get owner 'required))
             (name (car (last segments))))
        (seq-some
         (lambda (n)
           (equal (json-schema-edit-model-string-value n) name))
         (json-schema-edit-model-elements required))))))

(defun json-schema-edit-header-breadcrumb ()
  "Return the breadcrumb string for point."
  (let* ((model (json-schema-edit-model-current))
         (segments
          (json-schema-edit-model-schema-pointer-at model (point)))
         (node (json-schema-edit-model-node-at-pointer model segments)))
    (pcase json-schema-edit-breadcrumb-style
      ('schema
       (json-schema-edit-model-pointer-fragment segments))
      (_
       (let ((path
              (string-join
               (json-schema-edit-header-instance-path model segments)
               json-schema-edit-breadcrumb-separator))
             (type
              (and node (json-schema-edit-header-type-string node)))
             (required (json-schema-edit-header--required-p model segments)))
         (concat path
                 (when type (concat " : " type))
                 (when required " *")))))))

(defvar-local json-schema-edit-header--cache nil
  "Cached breadcrumb as (POINT TICK STRING).")

(defun json-schema-edit-header-line ()
  "Return the header line for `json-schema-edit-mode'."
  (when json-schema-edit-breadcrumb-style
    (let ((key
           (list
            (point)
            (buffer-chars-modified-tick)
            json-schema-edit-breadcrumb-style)))
      (unless (equal (car json-schema-edit-header--cache) key)
        (setq json-schema-edit-header--cache
              (cons key
                    (condition-case nil
                        (json-schema-edit-header-breadcrumb)
                      (error "")))))
      (concat " " (cdr json-schema-edit-header--cache)))))

;;;; Commands

(defun json-schema-edit-toggle-breadcrumb-style ()
  "Cycle the header line through instance path, schema pointer and off."
  (interactive)
  (setq-local json-schema-edit-breadcrumb-style
              (pcase json-schema-edit-breadcrumb-style
                ('instance 'schema)
                ('schema nil)
                (_ 'instance)))
  (force-mode-line-update))

(provide 'json-schema-edit-header)
;;; json-schema-edit-header.el ends here
