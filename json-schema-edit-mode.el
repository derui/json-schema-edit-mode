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
;; - an outline of the data shape (`C-c ; l'), also used by imenu
;; - constraints edited by type, and routine transformations such as
;;   nullable, array, wrapping in oneOf and moving to `$defs'
;;
;; Every operation is in the transient menu on `C-c ;'.
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
(require 'json-schema-edit-header)
(require 'json-schema-edit-edit)
(require 'json-schema-edit-lint)
(require 'json-schema-edit-outline)
(require 'json-schema-edit-transform)
(require 'json-schema-edit-constraint)
(require 'json-schema-edit-menu)

;;;; Commands

(defun json-schema-edit-copy-pointer ()
  "Copy the JSON Pointer (URI fragment) of the schema at point."
  (interactive)
  (let ((ptr
         (json-schema-edit-model-pointer-fragment
          (json-schema-edit-edit-schema-pointer))))
    (kill-new ptr)
    (message "Copied %s" ptr)))

;;;; Mode

(defvar json-schema-edit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `C-c ;' is a binding a minor mode may claim by convention;
    ;; `C-c C-s' belongs to the major mode.
    (define-key map (kbd "C-c ;") #'json-schema-edit-dispatch)
    (define-key map (kbd "M-p") #'json-schema-edit-move-property-up)
    (define-key map
                (kbd "M-n")
                #'json-schema-edit-move-property-down)
    map)
  "Keymap for `json-schema-edit-mode'.")

(defun json-schema-edit--context-menu (menu _click)
  "Add JSON Schema operations to the context MENU.
See `context-menu-functions'."
  (define-key-after menu [json-schema-edit-separator]
    '(menu-item "--"))
  (define-key-after menu [json-schema-edit-outline]
    '(menu-item "Outline" json-schema-edit-outline))
  (define-key-after menu [json-schema-edit-add-property]
    '(menu-item "Add property" json-schema-edit-add-property))
  (define-key-after menu [json-schema-edit-rename-property]
    '(menu-item "Rename property" json-schema-edit-rename-property))
  (define-key-after menu [json-schema-edit-toggle-required]
    '(menu-item "Toggle required" json-schema-edit-toggle-required))
  (define-key-after menu [json-schema-edit-change-type]
    '(menu-item "Change type" json-schema-edit-change-type))
  (define-key-after menu [json-schema-edit-constraints]
    '(menu-item "Constraints..." json-schema-edit-constraints))
  (define-key-after menu [json-schema-edit-find-references]
    '(menu-item "Find references" xref-find-references))
  menu)

(defconst json-schema-edit--header-line-format
  '(:eval (json-schema-edit-header-line))
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
        (json-schema-edit-model-parser)
        (add-hook 'xref-backend-functions #'json-schema-edit-xref-backend nil t)
        (add-hook 'eldoc-documentation-functions
                  #'json-schema-edit-xref-eldoc-function nil t)
        (add-hook 'flymake-diagnostic-functions #'json-schema-edit-lint-flymake nil t)
        (add-hook 'context-menu-functions #'json-schema-edit--context-menu nil t)
        (setq-local imenu-create-index-function #'json-schema-edit-outline-imenu-index)
        (setq-local header-line-format
                    (if (listp header-line-format)
                        (cons json-schema-edit--header-line-format
                              (remq json-schema-edit--header-line-format header-line-format))
                      (list json-schema-edit--header-line-format header-line-format))))
    (remove-hook 'xref-backend-functions #'json-schema-edit-xref-backend t)
    (remove-hook 'eldoc-documentation-functions #'json-schema-edit-xref-eldoc-function t)
    (remove-hook 'flymake-diagnostic-functions #'json-schema-edit-lint-flymake t)
    (remove-hook 'context-menu-functions #'json-schema-edit--context-menu t)
    (remove-hook 'after-change-functions #'json-schema-edit-outline--schedule t)
    (kill-local-variable 'imenu-create-index-function)
    (setq-local header-line-format
                (if (listp header-line-format)
                    (delq nil
                          (remq json-schema-edit--header-line-format header-line-format))
                  header-line-format))
    (when (null header-line-format)
      (kill-local-variable 'header-line-format))))

;;;###autoload
(defun json-schema-edit-mode-maybe ()
  "Enable `json-schema-edit-mode' when the buffer looks like a JSON Schema."
  (when (and
         (treesit-ready-p 'json t)
         (json-schema-edit-model-schema-p
          (json-schema-edit-model-current)))
    (json-schema-edit-mode 1)))

(defconst json-schema-edit--new-schema-versions
  '(("2020-12" . "https://json-schema.org/draft/2020-12/schema")
    ("draft-07" . "https://json-schema.org/draft-07/schema#"))
  "Schema versions `json-schema-edit-new' can start from.")

;;;###autoload
(defun json-schema-edit-new (version title)
  "Create a new schema buffer of VERSION titled TITLE.
VERSION is either \"2020-12\" or \"draft-07\".  The schema starts as an
object with empty `properties'."
  (interactive
   (list
    (completing-read "Schema version: "
                     (mapcar #'car json-schema-edit--new-schema-versions)
                     nil t nil nil "2020-12")
    (read-string "Title: ")))
  (let ((uri
         (cdr (assoc version json-schema-edit--new-schema-versions))))
    (unless uri
      (user-error "Unsupported schema version: %s" version))
    (switch-to-buffer
     (generate-new-buffer (format "*%s.schema.json*" title)))
    (insert
     (format "{\n  \"$schema\": %s,\n  \"title\": %s,\n  \"type\": \"object\",\n  \"properties\": {}\n}\n"
             (json-schema-edit-edit--literal uri)
             (json-schema-edit-edit--literal title)))
    (json-ts-mode)
    (json-schema-edit-mode 1)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(provide 'json-schema-edit-mode)
;;; json-schema-edit-mode.el ends here
