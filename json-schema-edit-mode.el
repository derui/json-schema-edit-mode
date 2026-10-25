;;; json-schema-edit-mode.el --- Minor mode for writing JSON Schema  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tools
;; URL: https://github.com/derui/json-schema-edit-mode
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; `json-schema-edit-mode' is a minor mode on top of `json-ts-mode' that
;; helps writing JSON Schema itself:
;;
;; - `M-.' / `M-?' on `$ref' (xref backend)
;; - eldoc summary of the `$ref' target
;; - a breadcrumb in the header line showing where in the data the
;;   schema at point applies
;; - schema-specific lint as a flymake backend
;; - adding and renaming properties with `required' kept in sync, and
;;   renaming definitions with every `$ref' rewritten
;;
;; Completion and metaschema validation are left to an LSP server such
;; as vscode-json-languageserver.
;;
;; To enable it automatically for schema files:
;;
;;   (add-hook 'json-ts-mode-hook #'json-schema-edit-mode-maybe)

;;; Code:

(require 'json-schema-edit-model)
(require 'json-schema-edit-xref)
(require 'json-schema-edit-edit)
(require 'json-schema-edit-lint)

(defcustom json-schema-edit-breadcrumb-style 'instance
  "How the header line shows the location at point.
`instance' shows the path of the data the schema describes, such as
\"User › address › zip\"; `schema' shows the JSON Pointer in the
schema; nil disables the header line."
  :type '(choice (const :tag "Instance path" instance)
                 (const :tag "Schema pointer" schema)
                 (const :tag "None" nil))
  :group 'json-schema-edit)

(defcustom json-schema-edit-breadcrumb-separator " › "
  "Separator between breadcrumb segments."
  :type 'string
  :group 'json-schema-edit)

;;;; Breadcrumb

(defconst json-schema-edit--combinators '("oneOf" "anyOf" "allOf")
  "Keywords whose array elements are alternative subschemas.")

(defun json-schema-edit--branch-name (node index)
  "Return a name for the combinator branch NODE at INDEX."
  (or (json-schema-edit-model-get-string node "title")
      (when-let* ((ref (json-schema-edit-model-get-string node "$ref")))
        (car (last (split-string ref "/"))))
      (format "#%d" (1+ index))))

(defun json-schema-edit--root-name ()
  "Return the name of the root schema."
  (or (json-schema-edit-model-get-string (json-schema-edit-model-root) "title")
      (and buffer-file-name
           (file-name-base (string-remove-suffix ".schema" (file-name-base buffer-file-name))))
      "root"))

(defun json-schema-edit-instance-path (segments)
  "Convert schema pointer SEGMENTS to a list of instance path labels.
The first label names the root schema, or the definition when SEGMENTS
point into `$defs' / `definitions'."
  (let ((labels (list (json-schema-edit--root-name)))
        (node (json-schema-edit-model-root)))
    (while segments
      (let ((seg (pop segments)))
        (pcase seg
          ((or "$defs" "definitions")
           (when segments
             (let ((name (pop segments)))
               (setq labels (list name)
                     node (json-schema-edit-model-node-at-pointer
                           (list seg name) node)))
             (setq seg nil)))
          ((or "properties" "patternProperties" "dependentSchemas")
           (when segments
             (let ((name (pop segments)))
               (push (if (equal seg "patternProperties") (format "/%s/" name) name)
                     labels)
               (setq node (json-schema-edit-model-node-at-pointer (list seg name) node)))
             (setq seg nil)))
          ((or "items" "additionalItems" "contains")
           (push "[]" labels))
          ("prefixItems"
           (when segments
             (let ((idx (pop segments)))
               (push (format "[%s]" idx) labels)
               (setq node (json-schema-edit-model-node-at-pointer (list seg idx) node)))
             (setq seg nil)))
          ("additionalProperties" (push "{*}" labels))
          ((pred (lambda (s) (member s json-schema-edit--combinators)))
           (when segments
             (let* ((idx (pop segments))
                    (branch (json-schema-edit-model-node-at-pointer (list seg idx) node))
                    (i (if (integerp idx) idx (string-to-number idx))))
               (push (format "‹%s: %s›" seg (json-schema-edit--branch-name branch i))
                     labels)
               (setq node branch))
             (setq seg nil))))
        (when seg
          (setq node (json-schema-edit-model-node-at-pointer (list seg) node)))))
    (nreverse labels)))

(defun json-schema-edit--required-p (segments)
  "Return non-nil if the property at SEGMENTS is listed in `required'."
  (let ((n (length segments)))
    (when (and (>= n 2) (equal (nth (- n 2) segments) "properties"))
      (let* ((owner (json-schema-edit-model-node-at-pointer (butlast segments 2)))
             (required (json-schema-edit-model-get owner "required"))
             (name (car (last segments))))
        (and required
             (equal (treesit-node-type required) "array")
             (seq-some (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                       (treesit-node-children required t)))))))

(defun json-schema-edit-breadcrumb ()
  "Return the breadcrumb string for point."
  (let* ((segments (json-schema-edit-model-schema-pointer-at))
         (node (json-schema-edit-model-node-at-pointer segments)))
    (pcase json-schema-edit-breadcrumb-style
      ('schema (json-schema-edit-model-pointer-fragment segments))
      (_
       (let ((path (string-join (json-schema-edit-instance-path segments)
                                json-schema-edit-breadcrumb-separator))
             (type (and node (json-schema-edit-model-type-string node)))
             (required (json-schema-edit--required-p segments)))
         (concat path
                 (when type (concat " : " type))
                 (when required " (必須)")))))))

(defvar-local json-schema-edit--breadcrumb-cache nil
  "Cached breadcrumb as (POINT TICK STRING).")

(defun json-schema-edit--header-line ()
  "Return the header line for `json-schema-edit-mode'."
  (when json-schema-edit-breadcrumb-style
    (let ((key (list (point) (buffer-chars-modified-tick)
                     json-schema-edit-breadcrumb-style)))
      (unless (equal (car json-schema-edit--breadcrumb-cache) key)
        (setq json-schema-edit--breadcrumb-cache
              (cons key (condition-case nil
                            (json-schema-edit-breadcrumb)
                          (error "")))))
      (concat " " (cdr json-schema-edit--breadcrumb-cache)))))

;;;; Commands

(defun json-schema-edit-toggle-breadcrumb-style ()
  "Toggle the header line between the instance path and the schema pointer."
  (interactive)
  (setq-local json-schema-edit-breadcrumb-style
              (if (eq json-schema-edit-breadcrumb-style 'schema) 'instance 'schema))
  (force-mode-line-update))

(defun json-schema-edit-copy-pointer ()
  "Copy the JSON Pointer (URI fragment) of the schema at point."
  (interactive)
  (let ((ptr (json-schema-edit-model-pointer-fragment
              (json-schema-edit-model-schema-pointer-at))))
    (kill-new ptr)
    (message "Copied %s" ptr)))

;;;; Mode

(defvar json-schema-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s a") #'json-schema-edit-add-property)
    (define-key map (kbd "C-c C-s r") #'json-schema-edit-rename-property)
    (define-key map (kbd "C-c C-s d") #'json-schema-edit-rename-definition)
    (define-key map (kbd "C-c C-s b") #'json-schema-edit-toggle-breadcrumb-style)
    (define-key map (kbd "C-c C-s w") #'json-schema-edit-copy-pointer)
    map)
  "Keymap for `json-schema-edit-mode'.")

(defconst json-schema-edit--header-line-format
  '(:eval (json-schema-edit--header-line))
  "Header line element added by `json-schema-edit-mode'.")

;;;###autoload
(define-minor-mode json-schema-edit-mode
  "Minor mode for writing JSON Schema.

\\{json-schema-edit-mode-map}"
  :lighter " JSchema"
  :keymap json-schema-edit-mode-map
  (if json-schema-edit-mode
      (progn
        (unless (treesit-ready-p 'json)
          (setq json-schema-edit-mode nil)
          (user-error "The json tree-sitter grammar is not available"))
        (json-schema-edit-model--parser)
        (add-hook 'xref-backend-functions #'json-schema-edit-xref-backend nil t)
        (add-hook 'eldoc-documentation-functions
                  #'json-schema-edit-xref-eldoc-function nil t)
        (add-hook 'flymake-diagnostic-functions #'json-schema-edit-lint-flymake nil t)
        (setq-local header-line-format
                    (if (listp header-line-format)
                        (cons json-schema-edit--header-line-format
                              (remq json-schema-edit--header-line-format header-line-format))
                      (list json-schema-edit--header-line-format header-line-format))))
    (remove-hook 'xref-backend-functions #'json-schema-edit-xref-backend t)
    (remove-hook 'eldoc-documentation-functions #'json-schema-edit-xref-eldoc-function t)
    (remove-hook 'flymake-diagnostic-functions #'json-schema-edit-lint-flymake t)
    (setq-local header-line-format
                (if (listp header-line-format)
                    (delq nil (remq json-schema-edit--header-line-format header-line-format))
                  header-line-format))
    (when (null header-line-format)
      (kill-local-variable 'header-line-format))))

;;;###autoload
(defun json-schema-edit-mode-maybe ()
  "Enable `json-schema-edit-mode' when the buffer looks like a JSON Schema."
  (when (and (treesit-ready-p 'json t)
             (json-schema-edit-model-schema-p))
    (json-schema-edit-mode 1)))

(provide 'json-schema-edit-mode)
;;; json-schema-edit-mode.el ends here
