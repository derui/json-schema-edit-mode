;;; json-schema-edit-edit.el --- Semantic edits on JSON Schema  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Edits that keep the parts of a schema that refer to each other in
;; sync:
;;
;; - adding a property updates `properties' and `required' at once
;; - renaming a property updates `required', `dependentRequired' and
;;   `dependencies'
;; - renaming a definition rewrites every `$ref' to it, optionally in
;;   the other schema files too

;;; Code:

(require 'json)
(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-xref)

(defcustom json-schema-edit-indent-offset nil
  "Indentation offset used for inserted members.
When nil, `json-ts-mode-indent-offset' is used if bound, else 2."
  :type '(choice (const :tag "Default" nil) integer)
  :group 'json-schema-edit)

(defcustom json-schema-edit-property-types
  '("string" "number" "integer" "boolean" "object" "array" "null")
  "Types offered when adding a property."
  :type '(repeat string)
  :group 'json-schema-edit)

(defvar json-ts-mode-indent-offset)

;;;; Text primitives

(defun json-schema-edit-edit--offset ()
  "Return the indentation offset."
  (or json-schema-edit-indent-offset
      (and (boundp 'json-ts-mode-indent-offset) json-ts-mode-indent-offset)
      2))

(defun json-schema-edit-edit--literal (string)
  "Return STRING as a JSON string literal."
  (let ((json-encoding-pretty-print nil))
    (json-encode-string string)))

(defun json-schema-edit-edit--one-line-p (node)
  "Return non-nil if NODE starts and ends on the same line."
  (save-excursion
    (goto-char (treesit-node-start node))
    (<= (treesit-node-end node) (line-end-position))))

(defun json-schema-edit-edit--column (pos)
  "Return the column of POS."
  (save-excursion (goto-char pos) (current-column)))

(defun json-schema-edit-edit--indent-text (text indent)
  "Prefix every line but the first of TEXT with INDENT spaces."
  (string-replace "\n" (concat "\n" (make-string indent ?\s)) text))

(defun json-schema-edit-edit--object-text (members)
  "Return a multi-line object holding MEMBERS, a list of (KEY . VALUE-TEXT).
Nested lines are indented relative to the opening brace."
  (let ((pad (make-string (json-schema-edit-edit--offset) ?\s)))
    (concat "{\n"
            (mapconcat (lambda (m)
                         (concat pad (json-schema-edit-edit--literal (car m)) ": "
                                 (json-schema-edit-edit--indent-text
                                  (cdr m) (json-schema-edit-edit--offset))))
                       members ",\n")
            "\n}")))

(defun json-schema-edit-edit--append (node children text)
  "Append TEXT to the container NODE whose elements are CHILDREN.
Return the position where TEXT was inserted."
  (save-excursion
    (cond
     ((and children (json-schema-edit-edit--one-line-p node))
      (goto-char (treesit-node-end (car (last children))))
      (insert ", ")
      (prog1 (point) (insert text)))
     (children
      (let ((indent (json-schema-edit-edit--column
                     (treesit-node-start (car children)))))
        (goto-char (treesit-node-end (car (last children))))
        (insert ",\n" (make-string indent ?\s))
        (prog1 (point)
          (insert (json-schema-edit-edit--indent-text text indent)))))
     (t
      (let* ((base (save-excursion
                     (goto-char (treesit-node-start node))
                     (current-indentation)))
             (indent (+ base (json-schema-edit-edit--offset))))
        ;; Replace whatever is between the brackets (whitespace, comments
        ;; are not expected in an empty container).
        (delete-region (1+ (treesit-node-start node)) (1- (treesit-node-end node)))
        (goto-char (1+ (treesit-node-start node)))
        (insert "\n" (make-string indent ?\s))
        (prog1 (point)
          (insert (json-schema-edit-edit--indent-text text indent)
                  "\n" (make-string base ?\s))))))))

(defun json-schema-edit-edit-add-member (object key value-text)
  "Add KEY with VALUE-TEXT at the end of the OBJECT node.
Return the position of the new key."
  (json-schema-edit-edit--append
   object (json-schema-edit-model-object-pairs object)
   (concat (json-schema-edit-edit--literal key) ": " value-text)))

(defun json-schema-edit-edit-append-element (array value-text)
  "Append VALUE-TEXT at the end of the ARRAY node."
  (json-schema-edit-edit--append
   array (json-schema-edit-model--value-children array) value-text))

(defun json-schema-edit-edit--apply-replacements (replacements)
  "Apply REPLACEMENTS, a list of (BEG END TEXT), in the current buffer.
They are applied from the end of the buffer so positions stay valid."
  (save-excursion
    (dolist (r (sort (copy-sequence replacements)
                     (lambda (a b) (> (car a) (car b)))))
      (goto-char (nth 0 r))
      (delete-region (nth 0 r) (nth 1 r))
      (insert (nth 2 r)))))

(defmacro json-schema-edit-edit--with-undo (&rest body)
  "Evaluate BODY so that its changes are undone as one unit."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "handle")))
    `(let ((,handle (prepare-change-group)))
       (unwind-protect
           (progn (activate-change-group ,handle) ,@body)
         (accept-change-group ,handle)
         (undo-amalgamate-change-group ,handle)))))

(defun json-schema-edit-edit--node-replacement (node text)
  "Return a replacement of NODE by TEXT."
  (list (treesit-node-start node) (treesit-node-end node) text))

;;;; Properties

(defun json-schema-edit-edit--property-pointer (&optional pos)
  "Return the pointer of the property schema at POS, or nil.
The pointer ends with \"properties\" and the property name."
  (let ((segments (json-schema-edit-model-schema-pointer-at pos)))
    (while (and segments
                (not (and (>= (length segments) 2)
                          (equal (car (last segments 2)) "properties"))))
      (setq segments (butlast segments)))
    segments))

(defun json-schema-edit-edit--object-schema-p (node)
  "Return non-nil if NODE is a schema that describes objects."
  (or (json-schema-edit-model-get node "properties")
      (let ((type (json-schema-edit-model-get node "type")))
        (and type
             (if (equal (treesit-node-type type) "array")
                 (seq-some (lambda (n) (equal (json-schema-edit-model-string-value n) "object"))
                           (json-schema-edit-model--value-children type))
               (equal (json-schema-edit-model-string-value type) "object"))))))

(defun json-schema-edit-edit--owner-pointer (&optional pos)
  "Return the pointer of the object schema new properties go into at POS.
That is the schema at POS when it describes objects, otherwise the
owner of the property at POS."
  (let* ((segments (json-schema-edit-model-schema-pointer-at pos))
         (node (json-schema-edit-model-node-at-pointer segments)))
    (if (json-schema-edit-edit--object-schema-p node)
        segments
      (if-let* ((prop (json-schema-edit-edit--property-pointer pos)))
          (butlast prop 2)
        (user-error "No object schema at point")))))

(defun json-schema-edit-edit--names (array)
  "Return the strings in the ARRAY node."
  (when (and array (equal (treesit-node-type array) "array"))
    (delq nil (mapcar #'json-schema-edit-model-string-value
                      (json-schema-edit-model--value-children array)))))

(defun json-schema-edit-edit--type-schema (type)
  "Return the schema text for TYPE.
A TYPE that looks like a reference becomes a `$ref'."
  (let ((pair (if (or (string-prefix-p "#" type) (string-search ".json" type))
                  (cons "$ref" type)
                (cons "type" type))))
    (concat "{ " (json-schema-edit-edit--literal (car pair)) ": "
            (json-schema-edit-edit--literal (cdr pair)) " }")))

(defun json-schema-edit-edit--read-type ()
  "Read a type or a `$ref' target."
  (completing-read "Type or $ref: "
                   (append json-schema-edit-property-types
                           (json-schema-edit-xref--definition-names))
                   nil nil nil nil "string"))

(defun json-schema-edit-add-property (owner name type required)
  "Add the property NAME of TYPE to the object schema at OWNER.
OWNER is a pointer.  When REQUIRED is non-nil NAME is also added to
`required'.  Interactively, OWNER is the object schema at point (or the
one holding the property at point)."
  (interactive
   (let* ((owner (json-schema-edit-edit--owner-pointer))
          (name (read-string "Property name: ")))
     (list owner name (json-schema-edit-edit--read-type) (y-or-n-p "Required? "))))
  (when (string-empty-p name)
    (user-error "Empty property name"))
  (let ((props (json-schema-edit-model-node-at-pointer (append owner '("properties")))))
    (when (json-schema-edit-model-get props name)
      (user-error "Property %s already exists" name))
    (json-schema-edit-edit--with-undo
      (let ((value (json-schema-edit-edit--type-schema type)))
        (if (equal (treesit-node-type props) "object")
            (json-schema-edit-edit-add-member props name value)
          (json-schema-edit-edit-add-member
           (json-schema-edit-model-node-at-pointer owner) "properties"
           (json-schema-edit-edit--object-text (list (cons name value))))))
      (when required
        (json-schema-edit-edit--add-required owner name)))
    (when-let* ((node (json-schema-edit-model-node-at-pointer
                       (append owner (list "properties" name)))))
      (goto-char (treesit-node-start (treesit-node-parent node))))))

(defun json-schema-edit-edit--add-required (owner name)
  "Add NAME to `required' of the object schema at OWNER."
  (let* ((node (json-schema-edit-model-node-at-pointer owner))
         (required (json-schema-edit-model-get node "required")))
    (cond
     ((member name (json-schema-edit-edit--names required)))
     ((and required (equal (treesit-node-type required) "array"))
      (json-schema-edit-edit-append-element
       required (json-schema-edit-edit--literal name)))
     (t
      (json-schema-edit-edit-add-member
       node "required" (concat "[" (json-schema-edit-edit--literal name) "]"))))))

(defun json-schema-edit-edit--rename-replacements (owner old new)
  "Return replacements renaming the property OLD to NEW in the OWNER node.
The key in `properties' and the names in `required',
`dependentRequired' and `dependencies' are covered."
  (let ((new-lit (json-schema-edit-edit--literal new))
        result)
    (cl-flet ((names-in (array)
                (when (and array (equal (treesit-node-type array) "array"))
                  (dolist (n (json-schema-edit-model--value-children array))
                    (when (equal (json-schema-edit-model-string-value n) old)
                      (push (json-schema-edit-edit--node-replacement n new-lit) result)))))
              (key-of (object)
                (dolist (pair (json-schema-edit-model-object-pairs object))
                  (when (equal (json-schema-edit-model-pair-key pair) old)
                    (push (json-schema-edit-edit--node-replacement
                           (treesit-node-child-by-field-name pair "key") new-lit)
                          result)))))
      (key-of (json-schema-edit-model-get owner "properties"))
      (names-in (json-schema-edit-model-get owner "required"))
      (dolist (kw '("dependentRequired" "dependencies"))
        (let ((deps (json-schema-edit-model-get owner kw)))
          (key-of deps)
          (dolist (pair (json-schema-edit-model-object-pairs deps))
            (names-in (json-schema-edit-model-pair-value pair))))))
    result))

(defun json-schema-edit-rename-property (pointer new)
  "Rename the property at POINTER to NEW, keeping `required' in sync.
POINTER ends with \"properties\" and the current name."
  (interactive
   (let ((pointer (or (json-schema-edit-edit--property-pointer)
                      (user-error "No property at point"))))
     (list pointer
           (read-string (format "Rename property %s to: " (car (last pointer)))
                        (car (last pointer))))))
  (let* ((old (car (last pointer)))
         (owner-ptr (butlast pointer 2))
         (owner (json-schema-edit-model-node-at-pointer owner-ptr)))
    (cond
     ((equal old new))
     ((string-empty-p new) (user-error "Empty property name"))
     ((json-schema-edit-model-get (json-schema-edit-model-get owner "properties") new)
      (user-error "Property %s already exists" new))
     (t
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit--apply-replacements
         (json-schema-edit-edit--rename-replacements owner old new)))))))

;;;; Definitions

(defun json-schema-edit-edit--definition-pointer (&optional pos)
  "Return (KEYWORD NAME) of the definition at POS, or nil.
On a local `$ref' the definition it points to is used."
  (let ((segments (or (when-let* ((ref (json-schema-edit-model-ref-at pos))
                                  (res (json-schema-edit-model-resolve
                                        (json-schema-edit-model-ref-value ref))))
                        (unless (map-elt res :file)
                          (map-elt res :pointer)))
                      (json-schema-edit-model-pointer-at pos))))
    (when (and (>= (length segments) 2)
               (member (car segments) '("$defs" "definitions")))
      (list (nth 0 segments) (nth 1 segments)))))

(defun json-schema-edit-edit--same-file-p (a b)
  "Return non-nil if the file names A and B (possibly nil) are the same."
  (if (and a b) (file-equal-p a b) (eq a b)))

(defun json-schema-edit-edit--ref-rewrite (value file prefix new-prefix)
  "Return VALUE rewritten from PREFIX to NEW-PREFIX, or nil.
VALUE is a `$ref' string; it is rewritten when it resolves into PREFIX
of FILE (nil meaning the current buffer) through a JSON Pointer."
  (pcase-let ((`(,resource . ,fragment) (json-schema-edit-model-split-ref value)))
    (when (and fragment (string-prefix-p "/" fragment))
      (let ((segments (json-schema-edit-model-pointer-decode fragment))
            (res (json-schema-edit-model-resolve value)))
        (when (and res
                   (json-schema-edit-edit--same-file-p
                    (or (map-elt res :file) buffer-file-name)
                    (or file buffer-file-name))
                   (equal (seq-take segments (length prefix)) prefix)
                   (equal (seq-take (json-schema-edit-model-normalize-segments
                                     (map-elt res :pointer))
                                    (length prefix))
                          prefix))
          (concat resource
                  (json-schema-edit-model-pointer-fragment
                   (append new-prefix (seq-drop segments (length prefix))))))))))

(defun json-schema-edit-edit--ref-replacements (file prefix new-prefix)
  "Return replacements for the `$ref's of this buffer into PREFIX of FILE."
  (delq nil
        (mapcar (lambda (ref)
                  (when-let* ((new (json-schema-edit-edit--ref-rewrite
                                    (json-schema-edit-model-ref-value ref)
                                    file prefix new-prefix)))
                    (list (json-schema-edit-model-ref-beg ref)
                          (json-schema-edit-model-ref-end ref)
                          (json-schema-edit-edit--literal new))))
                (json-schema-edit-model-refs))))

(defun json-schema-edit-edit--other-files (prefix)
  "Return the other schema files with `$ref's into PREFIX of this file."
  (when-let* ((self buffer-file-name))
    (seq-filter
     (lambda (file)
       (and (not (file-equal-p file self))
            (file-readable-p file)
            (json-schema-edit-model-with-file file
              (json-schema-edit-edit--ref-replacements self prefix prefix))))
     (funcall json-schema-edit-reference-files-function))))

(defun json-schema-edit-rename-definition (definition new)
  "Rename DEFINITION, a list (KEYWORD NAME), to NEW.
Every `$ref' to it in this buffer is rewritten.  When other schema
files refer to it, you are asked whether to update them as well; they
are visited and modified but not saved."
  (interactive
   (let ((def (or (json-schema-edit-edit--definition-pointer)
                  (user-error "No definition at point"))))
     (list def (read-string (format "Rename definition %s to: " (nth 1 def))
                            (nth 1 def)))))
  (pcase-let* ((`(,keyword ,old) definition)
               (prefix (list keyword old))
               (new-prefix (list keyword new))
               (defs (json-schema-edit-model-get (json-schema-edit-model-root) keyword)))
    (cond
     ((equal old new))
     ((string-empty-p new) (user-error "Empty definition name"))
     ((json-schema-edit-model-get defs new)
      (user-error "Definition %s already exists" new))
     (t
      (let* ((self buffer-file-name)
             (others (json-schema-edit-edit--other-files prefix))
             (pair (treesit-node-parent (json-schema-edit-model-get defs old)))
             ;; Collected before this buffer changes, since resolving
             ;; them needs the old name.
             (external
              (when (and others
                         (y-or-n-p (format "Update references in %d other file(s)? "
                                           (length others))))
                (mapcar (lambda (file)
                          (with-current-buffer (find-file-noselect file)
                            (json-schema-edit-model--parser)
                            (cons (current-buffer)
                                  (json-schema-edit-edit--ref-replacements
                                   self prefix new-prefix))))
                        others))))
        (json-schema-edit-edit--with-undo
          (json-schema-edit-edit--apply-replacements
           (cons (json-schema-edit-edit--node-replacement
                  (treesit-node-child-by-field-name pair "key")
                  (json-schema-edit-edit--literal new))
                 (json-schema-edit-edit--ref-replacements nil prefix new-prefix))))
        (pcase-dolist (`(,buf . ,replacements) external)
          (with-current-buffer buf
            (json-schema-edit-edit--with-undo
              (json-schema-edit-edit--apply-replacements replacements)))))))))

(provide 'json-schema-edit-edit)
;;; json-schema-edit-edit.el ends here
