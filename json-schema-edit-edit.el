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
;; - toggling `required' of a property
;; - changing the type of a schema, offering to drop the keywords that
;;   no longer apply

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

(defun json-schema-edit-edit--compact-object-text (members)
  "Return an object holding MEMBERS, a list of (KEY . VALUE-TEXT).
It is written on one line when short and single-line, else like
`json-schema-edit-edit--object-text'."
  (let ((inline (concat "{ "
                        (mapconcat (lambda (m)
                                     (concat (json-schema-edit-edit--literal (car m))
                                             ": " (cdr m)))
                                   members ", ")
                        " }")))
    (if (and (<= (length inline) 60) (not (string-search "\n" inline)))
        inline
      (json-schema-edit-edit--object-text members))))

(defun json-schema-edit-edit--array-text (elements)
  "Return an array of ELEMENTS (texts), on one line when short."
  (let ((inline (concat "[" (string-join elements ", ") "]"))
        (pad (make-string (json-schema-edit-edit--offset) ?\s)))
    (if (and (<= (length inline) 60) (not (string-search "\n" inline)))
        inline
      (concat "[\n"
              (mapconcat (lambda (e)
                           (concat pad (json-schema-edit-edit--indent-text
                                        e (json-schema-edit-edit--offset))))
                         elements ",\n")
              "\n]"))))

(defun json-schema-edit-edit-node-text (node)
  "Return the text of NODE with its lines indented relative to its start."
  (let ((base (save-excursion
                (goto-char (treesit-node-start node))
                (current-indentation))))
    (replace-regexp-in-string
     (concat "\n" (make-string base ?\s)) "\n"
     (treesit-node-text node t))))

(defun json-schema-edit-edit-replace-node (node text)
  "Replace NODE by TEXT, indenting its later lines like NODE's line."
  (let ((indent (save-excursion
                  (goto-char (treesit-node-start node))
                  (current-indentation))))
    (save-excursion
      (goto-char (treesit-node-start node))
      (delete-region (treesit-node-start node) (treesit-node-end node))
      (insert (json-schema-edit-edit--indent-text text indent)))))

(defun json-schema-edit-edit-insert-after (node siblings text)
  "Insert TEXT after NODE, one of SIBLINGS in its container."
  (save-excursion
    (if (json-schema-edit-edit--one-line-p (treesit-node-parent node))
        (progn (goto-char (treesit-node-end node))
               (insert ", " text))
      (let ((indent (json-schema-edit-edit--column
                     (treesit-node-start (car siblings)))))
        (goto-char (treesit-node-end node))
        (insert ",\n" (make-string indent ?\s)
                (json-schema-edit-edit--indent-text text indent))))))

(defun json-schema-edit-edit-members (node)
  "Return the members of the object NODE as (KEY . RELATIVE-TEXT)."
  (mapcar (lambda (p)
            (cons (json-schema-edit-model-pair-key p)
                  (json-schema-edit-edit-node-text (json-schema-edit-model-pair-value p))))
          (json-schema-edit-model-object-pairs node)))

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

(defun json-schema-edit-edit--remove (node siblings)
  "Delete NODE, one of SIBLINGS in its container, with its separator."
  (let ((idx (seq-position siblings node #'treesit-node-eq)))
    (cond
     ((= (length siblings) 1)
      (let ((parent (treesit-node-parent node)))
        (delete-region (1+ (treesit-node-start parent)) (1- (treesit-node-end parent)))))
     ((< idx (1- (length siblings)))
      (delete-region (treesit-node-start node)
                     (treesit-node-start (nth (1+ idx) siblings))))
     (t
      (delete-region (treesit-node-end (nth (1- idx) siblings))
                     (treesit-node-end node))))))

(defun json-schema-edit-edit-remove-member (object key)
  "Remove KEY from the OBJECT node, if present."
  (let ((pairs (json-schema-edit-model-object-pairs object)))
    (when-let* ((pair (seq-find (lambda (p) (equal (json-schema-edit-model-pair-key p) key))
                                pairs)))
      (json-schema-edit-edit--remove pair pairs))))

(defun json-schema-edit-edit-remove-element (array element)
  "Remove the ELEMENT node from the ARRAY node."
  (json-schema-edit-edit--remove element (json-schema-edit-model--value-children array)))

(defun json-schema-edit-edit-set-member (object key value-text)
  "Set KEY of the OBJECT node to VALUE-TEXT, adding it when missing."
  (if-let* ((value (json-schema-edit-model-get object key)))
      (save-excursion
        (goto-char (treesit-node-start value))
        (delete-region (treesit-node-start value) (treesit-node-end value))
        (insert value-text))
    (json-schema-edit-edit-add-member object key value-text)))

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

(defun json-schema-edit-edit-required-p (pointer)
  "Return non-nil if the property at POINTER is listed in `required'."
  (let ((owner (json-schema-edit-model-node-at-pointer (butlast pointer 2))))
    (member (car (last pointer))
            (json-schema-edit-edit--names (json-schema-edit-model-get owner "required")))))

(defun json-schema-edit-toggle-required (pointer)
  "Toggle whether the property at POINTER is listed in `required'.
POINTER ends with \"properties\" and the property name.  An empty
`required' is removed."
  (interactive
   (list (or (json-schema-edit-edit--property-pointer)
             (user-error "No property at point"))))
  (let* ((owner-ptr (butlast pointer 2))
         (name (car (last pointer))))
    (json-schema-edit-edit--with-undo
      (if (not (json-schema-edit-edit-required-p pointer))
          (json-schema-edit-edit--add-required owner-ptr name)
        (let* ((owner (json-schema-edit-model-node-at-pointer owner-ptr))
               (required (json-schema-edit-model-get owner "required"))
               (elements (json-schema-edit-model--value-children required)))
          (if (seq-every-p (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                           elements)
              (json-schema-edit-edit-remove-member owner "required")
            (json-schema-edit-edit-remove-element
             required
             (seq-find (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                       elements))))))))

(defun json-schema-edit-change-type (pointer type)
  "Change the type of the schema at POINTER to TYPE.
A TYPE that looks like a reference replaces `type' with `$ref'.
Keywords that no longer apply are listed and removed on confirmation."
  (interactive
   (list (json-schema-edit-model-schema-pointer-at)
         (json-schema-edit-edit--read-type)))
  (let* ((ref-p (or (string-prefix-p "#" type) (string-search ".json" type)))
         (node (json-schema-edit-model-node-at-pointer pointer))
         (stale (unless ref-p
                  (seq-remove (lambda (k)
                                (json-schema-edit-model-keyword-applies-p k (list type)))
                              (mapcar #'json-schema-edit-model-pair-key
                                      (json-schema-edit-model-object-pairs node))))))
    (unless (equal (treesit-node-type node) "object")
      (user-error "No schema object at point"))
    ;; Ask before writing anything, so a negative answer leaves the
    ;; schema as it was.
    (let ((drop (and stale
                     (y-or-n-p
                      (format-message "Remove keywords that no longer apply (%s)? "
                                      (string-join stale ", "))))))
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit-set-member
         (json-schema-edit-model-node-at-pointer pointer)
         (if ref-p "$ref" "type") (json-schema-edit-edit--literal type))
        (json-schema-edit-edit-remove-member
         (json-schema-edit-model-node-at-pointer pointer) (if ref-p "type" "$ref"))
        (when drop
          (dolist (k stale)
            (json-schema-edit-edit-remove-member
             (json-schema-edit-model-node-at-pointer pointer) k)))))))

(defun json-schema-edit-edit--rename-replacements (owner old new)
  "Return replacements renaming the property OLD to NEW in the OWNER node.
The key in `properties' and the names in `required',
`dependentRequired', `dependencies' and `dependentSchemas' are
covered."
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
            (names-in (json-schema-edit-model-pair-value pair)))))
      ;; The values of `dependentSchemas' are schemas, not name arrays.
      (key-of (json-schema-edit-model-get owner "dependentSchemas")))
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

(defun json-schema-edit-edit--remove-name-references (owner-ptr name)
  "Remove references to the property NAME in the schema at OWNER-PTR.
Covers `required', `dependentRequired', `dependencies' and
`dependentSchemas'.  The owner is re-read after every edit, since an
edit invalidates the tree nodes."
  (cl-flet ((owner () (json-schema-edit-model-node-at-pointer owner-ptr)))
    (let ((required (json-schema-edit-model-get (owner) "required")))
      (when (member name (json-schema-edit-edit--names required))
        (if (equal (json-schema-edit-edit--names required) (list name))
            (json-schema-edit-edit-remove-member (owner) "required")
          (json-schema-edit-edit-remove-element
           required
           (seq-find (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                     (json-schema-edit-model--value-children required))))))
    (dolist (kw '("dependentRequired" "dependencies" "dependentSchemas"))
      ;; Remove NAME from the name arrays of KW.  One edit per
      ;; iteration, so the tree is re-read after every change.
      (cl-loop for pair = (seq-find
                           (lambda (p)
                             (let ((v (json-schema-edit-model-pair-value p)))
                               (and (equal (treesit-node-type v) "array")
                                    (member name (json-schema-edit-edit--names v)))))
                           (json-schema-edit-model-object-pairs
                            (json-schema-edit-model-get (owner) kw)))
               while pair
               do (let ((v (json-schema-edit-model-pair-value pair)))
                    (if (equal (json-schema-edit-edit--names v) (list name))
                        ;; A name array that became empty says nothing.
                        (json-schema-edit-edit-remove-member
                         (json-schema-edit-model-get (owner) kw)
                         (json-schema-edit-model-pair-key pair))
                      (json-schema-edit-edit-remove-element
                       v (seq-find
                          (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                          (json-schema-edit-model--value-children v))))))
      (json-schema-edit-edit-remove-member (json-schema-edit-model-get (owner) kw) name)
      ;; A keyword left with an empty object says nothing either.
      (when (and (json-schema-edit-model-get (owner) kw)
                 (null (json-schema-edit-model-object-pairs
                        (json-schema-edit-model-get (owner) kw))))
        (json-schema-edit-edit-remove-member (owner) kw)))))

(defun json-schema-edit-remove-property (pointer)
  "Remove the property at POINTER from its object schema.
POINTER ends with \"properties\" and the property name.  The name is
also removed from `required', `dependentRequired', `dependencies' and
`dependentSchemas'.  Called interactively, the removal is confirmed
first."
  (interactive
   (let ((pointer (or (json-schema-edit-edit--property-pointer)
                      (user-error "No property at point"))))
     (unless (y-or-n-p (format "Remove property %s? " (car (last pointer))))
       (user-error "Aborted"))
     (list pointer)))
  (let ((owner-ptr (butlast pointer 2))
        (name (car (last pointer))))
    (json-schema-edit-edit--with-undo
      (json-schema-edit-edit-remove-member
       (json-schema-edit-model-get
        (json-schema-edit-model-node-at-pointer owner-ptr) "properties")
       name)
      (json-schema-edit-edit--remove-name-references owner-ptr name))))

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

(defun json-schema-edit-edit--file-list-text (files)
  "Return a short, human readable list of FILES."
  (if (<= (length files) 3)
      (mapconcat #'file-relative-name files ", ")
    (format "%s, ... (%d files)"
            (mapconcat #'file-relative-name (seq-take files 3) ", ")
            (length files))))

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
                         (y-or-n-p (format "Update references in %s? "
                                           (json-schema-edit-edit--file-list-text others))))
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
              (json-schema-edit-edit--apply-replacements replacements))))
        (when external
          (message "Updated references in %s; save the buffers to keep the changes"
                   (json-schema-edit-edit--file-list-text
                    (delq nil (mapcar (lambda (e) (buffer-file-name (car e)))
                                      external))))))))))

(provide 'json-schema-edit-edit)
;;; json-schema-edit-edit.el ends here
