;;; json-schema-edit-menu.el --- Transient menus of json-schema-edit-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; `json-schema-edit-dispatch' (C-c ;) gathers every operation of the
;; mode; `json-schema-edit-combinator-menu' (C-c ; o) edits the
;; branches of oneOf / anyOf / allOf at point.

;;; Code:

(require 'transient)
(require 'json-schema-edit-model)
(require 'json-schema-edit-edit)
(require 'json-schema-edit-transform)
(require 'json-schema-edit-constraint)
(require 'json-schema-edit-outline)

(declare-function json-schema-edit-toggle-breadcrumb-style "json-schema-edit-header") (declare-function json-schema-edit-copy-pointer "json-schema-edit-mode") (declare-function json-schema-edit-new "json-schema-edit-mode")

(defun json-schema-edit-menu--combinator-desc ()
  "Return the header of the combinator menu for point."
  (pcase (ignore-errors (json-schema-edit-transform--combinator-at))
    (`(,owner ,kw ,i)
     (format "%s of %s%s"
             (propertize
              (symbol-name kw)
              'face 'font-lock-keyword-face)
             (json-schema-edit-model-pointer-fragment owner)
             (if i (format ", branch #%d" (1+ i)) "")))
    (_
     (propertize "No oneOf / anyOf / allOf at point" 'face 'warning))))

(transient-define-prefix json-schema-edit-combinator-menu
  ()
  "Edit the branches of the oneOf / anyOf / allOf at point."
  [:description json-schema-edit-menu--combinator-desc
                ["Branches"
                 ("a" "add" json-schema-edit-add-branch)
                 ("k" "remove" json-schema-edit-remove-branch)
                 ("y" "duplicate" json-schema-edit-duplicate-branch)
                 ("p" "move up" json-schema-edit-move-branch-up)
                 ("n" "move down" json-schema-edit-move-branch-down)
                 ]
                ["Shape"
                 ("s" "switch oneOf / anyOf / allOf" json-schema-edit-switch-combinator)
                 ("d" "set discriminating property" json-schema-edit-set-discriminator)
                 ("f" "factor out common properties" json-schema-edit-factor-common-properties)
                 ("N" "toggle nullable" json-schema-edit-toggle-nullable)
                 ]])

;;;###autoload (autoload 'json-schema-edit-dispatch "json-schema-edit-menu" nil t)
(transient-define-prefix json-schema-edit-dispatch
  ()
  "Operations of `json-schema-edit-mode'."
  [["View"
    ("n" "new schema" json-schema-edit-new)
    ("l" "outline" json-schema-edit-outline)
    ("b" "toggle breadcrumb style" json-schema-edit-toggle-breadcrumb-style)
    ("w" "copy JSON Pointer" json-schema-edit-copy-pointer)
    ]
   ["Property"
    ("a" "add" json-schema-edit-add-property)
    ("r" "rename" json-schema-edit-rename-property)
    ("k" "remove" json-schema-edit-remove-property)
    ("*" "toggle required" json-schema-edit-toggle-required)
    ("t" "change type" json-schema-edit-change-type)
    ("M-p" "move up" json-schema-edit-move-property-up)
    ("M-n" "move down" json-schema-edit-move-property-down)
    ("c" "constraints..." json-schema-edit-constraints)
    ]]
  [["Transform"
    ("N" "toggle nullable" json-schema-edit-toggle-nullable)
    ("[" "make array of it" json-schema-edit-wrap-array)
    ("W" "wrap in oneOf / anyOf / allOf" json-schema-edit-wrap-combinator)
    ("o" "oneOf / anyOf / allOf..." json-schema-edit-combinator-menu)
    ]
   ["Definitions"
    ("x" "extract to definitions" json-schema-edit-extract-definition)
    ("i" "inline $ref" json-schema-edit-inline-ref)
    ("d" "rename definition" json-schema-edit-rename-definition)
    ]])

(provide 'json-schema-edit-menu)
;;; json-schema-edit-menu.el ends here
