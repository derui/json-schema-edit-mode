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
    (goto-char (json-schema-edit-model-node-beg node))
    (<= (json-schema-edit-model-node-end node) (line-end-position))))

(defun json-schema-edit-edit--column (pos)
  "Return the column of POS."
  (save-excursion (goto-char pos) (current-column)))

(defun json-schema-edit-edit--indent-text (text indent)
  "Prefix every line but the first of TEXT with INDENT spaces."
  (string-replace "\n" (concat "\n" (make-string indent ?\s)) text))

(defun json-schema-edit-edit--key-literal (key)
  "Return KEY, a keyword symbol or a string, as a JSON string literal."
  (json-schema-edit-edit--literal (json-schema-edit-model-key-name key)))

(defun json-schema-edit-edit--object-text (members)
  "Return a multi-line object holding MEMBERS, a list of (KEY . VALUE-TEXT).
KEY is a keyword symbol or a string.  Nested lines are indented
relative to the opening brace."
  (let ((pad (make-string (json-schema-edit-edit--offset) ?\s)))
    (concat "{\n"
            (mapconcat (lambda (m)
                         (concat pad (json-schema-edit-edit--key-literal (car m)) ": "
                                 (json-schema-edit-edit--indent-text
                                  (cdr m) (json-schema-edit-edit--offset))))
                       members ",\n")
            "\n}")))

(defun json-schema-edit-edit--append (node text)
  "Append TEXT to the container NODE.
Return the position where TEXT was inserted."
  (let ((children (json-schema-edit-model-children node)))
    (save-excursion
      (cond
       ((and children (json-schema-edit-edit--one-line-p node))
        (goto-char (json-schema-edit-model-node-end (car (last children))))
        (insert ", ")
        (prog1 (point) (insert text)))
       (children
        (let ((indent (json-schema-edit-edit--column
                       (json-schema-edit-model-member-beg (car children)))))
          (goto-char (json-schema-edit-model-node-end (car (last children))))
          (insert ",\n" (make-string indent ?\s))
          (prog1 (point)
            (insert (json-schema-edit-edit--indent-text text indent)))))
       (t
        (let* ((beg (json-schema-edit-model-node-beg node))
               (base (save-excursion
                       (goto-char beg)
                       (current-indentation)))
               (indent (+ base (json-schema-edit-edit--offset))))
          ;; Replace whatever is between the brackets (whitespace, comments
          ;; are not expected in an empty container).
          (delete-region (1+ beg) (1- (json-schema-edit-model-node-end node)))
          (goto-char (1+ beg))
          (insert "\n" (make-string indent ?\s))
          (prog1 (point)
            (insert (json-schema-edit-edit--indent-text text indent)
                    "\n" (make-string base ?\s)))))))))

(defun json-schema-edit-edit--compact-object-text (members)
  "Return an object holding MEMBERS, a list of (KEY . VALUE-TEXT).
It is written on one line when short and single-line, else like
`json-schema-edit-edit--object-text'."
  (let ((inline (concat "{ "
                        (mapconcat (lambda (m)
                                     (concat (json-schema-edit-edit--key-literal (car m))
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
                (goto-char (json-schema-edit-model-node-beg node))
                (current-indentation))))
    (replace-regexp-in-string
     (concat "\n" (make-string base ?\s)) "\n"
     (json-schema-edit-model-node-text node))))

(defun json-schema-edit-edit-replace-node (node text)
  "Replace NODE by TEXT, indenting its later lines like NODE's line."
  (let* ((beg (json-schema-edit-model-node-beg node))
         (indent (save-excursion
                   (goto-char beg)
                   (current-indentation))))
    (save-excursion
      (goto-char beg)
      (delete-region beg (json-schema-edit-model-node-end node))
      (insert (json-schema-edit-edit--indent-text text indent)))))

(defun json-schema-edit-edit-insert-after (container node text)
  "Insert TEXT after NODE, one of the elements of CONTAINER."
  (save-excursion
    (if (json-schema-edit-edit--one-line-p container)
        (progn (goto-char (json-schema-edit-model-node-end node))
               (insert ", " text))
      (let ((indent (json-schema-edit-edit--column
                     (json-schema-edit-model-member-beg
                      (car (json-schema-edit-model-children container))))))
        (goto-char (json-schema-edit-model-node-end node))
        (insert ",\n" (make-string indent ?\s)
                (json-schema-edit-edit--indent-text text indent))))))

(defun json-schema-edit-edit-members (node)
  "Return the members of the object NODE as (KEY . RELATIVE-TEXT)."
  (mapcar (lambda (m)
            (cons (json-schema-edit-model-node-key m)
                  (json-schema-edit-edit-node-text m)))
          (json-schema-edit-model-members node)))

(defun json-schema-edit-edit-add-member (object key value-text)
  "Add KEY with VALUE-TEXT at the end of the OBJECT node.
KEY is a keyword symbol or a string.  Return the position of the new
key."
  (json-schema-edit-edit--append
   object (concat (json-schema-edit-edit--key-literal key) ": " value-text)))

(defun json-schema-edit-edit-append-element (array value-text)
  "Append VALUE-TEXT at the end of the ARRAY node."
  (json-schema-edit-edit--append array value-text))

(defun json-schema-edit-edit--remove (container node)
  "Delete NODE, a member or element of CONTAINER, with its separator."
  (let* ((siblings (json-schema-edit-model-children container))
         (idx (seq-position siblings node #'eq)))
    (cond
     ((= (length siblings) 1)
      (delete-region (1+ (json-schema-edit-model-node-beg container))
                     (1- (json-schema-edit-model-node-end container))))
     ((< idx (1- (length siblings)))
      (delete-region (json-schema-edit-model-member-beg node)
                     (json-schema-edit-model-member-beg (nth (1+ idx) siblings))))
     (t
      (delete-region (json-schema-edit-model-node-end (nth (1- idx) siblings))
                     (json-schema-edit-model-node-end node))))))

(defun json-schema-edit-edit-remove-member (object key)
  "Remove KEY, a keyword symbol or a string, from the OBJECT node, if present."
  (when-let* ((member (json-schema-edit-model-get object key)))
    (json-schema-edit-edit--remove object member)))

(defun json-schema-edit-edit-remove-element (array element)
  "Remove the ELEMENT node from the ARRAY node."
  (json-schema-edit-edit--remove array element))

(defun json-schema-edit-edit-set-member (object key value-text)
  "Set KEY of the OBJECT node to VALUE-TEXT, adding it when missing.
KEY is a keyword symbol or a string."
  (if-let* ((value (json-schema-edit-model-get object key)))
      (save-excursion
        (goto-char (json-schema-edit-model-node-beg value))
        (delete-region (json-schema-edit-model-node-beg value)
                       (json-schema-edit-model-node-end value))
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
  (list (json-schema-edit-model-node-beg node) (json-schema-edit-model-node-end node) text))

(defun json-schema-edit-edit--key-replacement (member text)
  "Return a replacement of the key of MEMBER by TEXT."
  (list (json-schema-edit-model-node-key-beg member)
        (json-schema-edit-model-node-key-end member)
        text))

(defun json-schema-edit-edit--member-text (member)
  "Return the text of MEMBER with its key."
  (buffer-substring-no-properties (json-schema-edit-model-member-beg member)
                                  (json-schema-edit-model-node-end member)))

(defun json-schema-edit-edit--model ()
  "Return the model of the current buffer."
  (json-schema-edit-model-current))

(defun json-schema-edit-edit--node (pointer)
  "Return the node at POINTER in the current buffer.
Call it again after every edit, since an edit makes nodes stale."
  (json-schema-edit-model-node-at-pointer (json-schema-edit-edit--model) pointer))

(defun json-schema-edit-edit-schema-pointer (&optional pos)
  "Return the pointer of the schema object at POS (default: point)."
  (json-schema-edit-model-schema-pointer-at (json-schema-edit-edit--model)
                                            (or pos (point))))

;;;; Properties

(defun json-schema-edit-edit--property-pointer (&optional pos)
  "Return the pointer of the property schema at POS, or nil.
The pointer ends with \"properties\" and the property name."
  (let ((segments (json-schema-edit-edit-schema-pointer pos)))
    (while (and segments
                (not (and (>= (length segments) 2)
                          (eq (json-schema-edit-model-keyword (car (last segments 2)))
                              'properties))))
      (setq segments (butlast segments)))
    segments))

(defun json-schema-edit-edit--object-schema-p (node)
  "Return non-nil if NODE is a schema that describes objects."
  (or (json-schema-edit-model-get node 'properties)
      (member "object" (json-schema-edit-model-declared-types node))))

(defun json-schema-edit-edit--owner-pointer (&optional pos)
  "Return the pointer of the object schema new properties go into at POS.
That is the schema at POS when it describes objects, otherwise the
owner of the property at POS."
  (let* ((segments (json-schema-edit-edit-schema-pointer pos))
         (node (json-schema-edit-edit--node segments)))
    (if (json-schema-edit-edit--object-schema-p node)
        segments
      (if-let* ((prop (json-schema-edit-edit--property-pointer pos)))
          (butlast prop 2)
        (user-error "No object schema at point")))))

(defun json-schema-edit-edit--names (array)
  "Return the strings in the ARRAY node."
  (delq nil (mapcar #'json-schema-edit-model-string-value
                    (json-schema-edit-model-elements array))))

(defun json-schema-edit-edit--find-string (array name)
  "Return the element of the ARRAY node that is the string NAME, or nil."
  (seq-find (lambda (n) (equal (json-schema-edit-model-string-value n) name))
            (json-schema-edit-model-elements array)))

(defun json-schema-edit-edit--type-schema (type)
  "Return the schema text for TYPE.
A TYPE that looks like a reference becomes a `$ref'."
  (let ((pair (if (or (string-prefix-p "#" type) (string-search ".json" type))
                  (cons '$ref type)
                (cons 'type type))))
    (concat "{ " (json-schema-edit-edit--key-literal (car pair)) ": "
            (json-schema-edit-edit--literal (cdr pair))
            (when (equal (cdr pair) "object") ", \"properties\": {}")
            " }")))

(defun json-schema-edit-edit--read-type ()
  "Read a type or a `$ref' target."
  (completing-read "Type or $ref: "
                   (append json-schema-edit-property-types
                           (json-schema-edit-xref--definition-names
                            (json-schema-edit-edit--model)))
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
  (let ((props (json-schema-edit-edit--node (append owner '(properties)))))
    (when (json-schema-edit-model-get props name)
      (user-error "Property %s already exists" name))
    (json-schema-edit-edit--with-undo
      (let ((value (json-schema-edit-edit--type-schema type)))
        (if (json-schema-edit-model-object-p props)
            (json-schema-edit-edit-add-member props name value)
          (json-schema-edit-edit-add-member
           (json-schema-edit-edit--node owner) 'properties
           (json-schema-edit-edit--object-text (list (cons name value))))))
      (when required
        (json-schema-edit-edit--add-required owner name)))
    (when-let* ((node (json-schema-edit-edit--node
                       (append owner (list 'properties name)))))
      (goto-char (json-schema-edit-model-member-beg node)))))

(defun json-schema-edit-edit--add-required (owner name)
  "Add NAME to `required' of the object schema at OWNER."
  (let* ((node (json-schema-edit-edit--node owner))
         (required (json-schema-edit-model-get node 'required)))
    (cond
     ((member name (json-schema-edit-edit--names required)))
     ((json-schema-edit-model-array-p required)
      (json-schema-edit-edit-append-element
       required (json-schema-edit-edit--literal name)))
     (t
      (json-schema-edit-edit-add-member
       node 'required (concat "[" (json-schema-edit-edit--literal name) "]"))))))

(defun json-schema-edit-edit-required-p (pointer)
  "Return non-nil if the property at POINTER is listed in `required'."
  (let ((owner (json-schema-edit-edit--node (butlast pointer 2))))
    (member (car (last pointer))
            (json-schema-edit-edit--names (json-schema-edit-model-get owner 'required)))))

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
        (let* ((owner (json-schema-edit-edit--node owner-ptr))
               (required (json-schema-edit-model-get owner 'required)))
          (if (seq-every-p (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                           (json-schema-edit-model-elements required))
              (json-schema-edit-edit-remove-member owner 'required)
            (json-schema-edit-edit-remove-element
             required (json-schema-edit-edit--find-string required name))))))))

(defun json-schema-edit-change-type (pointer type)
  "Change the type of the schema at POINTER to TYPE.
A TYPE that looks like a reference replaces `type' with `$ref'.
Keywords that no longer apply are listed and removed on confirmation."
  (interactive
   (list (json-schema-edit-edit-schema-pointer)
         (json-schema-edit-edit--read-type)))
  (let* ((ref-p (or (string-prefix-p "#" type) (string-search ".json" type)))
         (node (json-schema-edit-edit--node pointer))
         (stale (unless ref-p
                  (seq-remove (lambda (k)
                                (json-schema-edit-model-keyword-applies-p k (list type)))
                              (mapcar #'json-schema-edit-model-node-key
                                      (json-schema-edit-model-members node))))))
    (unless (json-schema-edit-model-object-p node)
      (user-error "No schema object at point"))
    ;; Ask before writing anything, so a negative answer leaves the
    ;; schema as it was.
    (let ((drop (and stale
                     (y-or-n-p
                      (format-message "Remove keywords that no longer apply (%s)? "
                                      (string-join stale ", "))))))
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit-set-member
         (json-schema-edit-edit--node pointer)
         (if ref-p '$ref 'type) (json-schema-edit-edit--literal type))
        (json-schema-edit-edit-remove-member
         (json-schema-edit-edit--node pointer) (if ref-p 'type '$ref))
        (when drop
          (dolist (k stale)
            (json-schema-edit-edit-remove-member
             (json-schema-edit-edit--node pointer) k)))))))

(defun json-schema-edit-edit--rename-replacements (owner old new)
  "Return replacements renaming the property OLD to NEW in the OWNER node.
The key in `properties' and the names in `required',
`dependentRequired', `dependencies' and `dependentSchemas' are
covered."
  (let ((new-lit (json-schema-edit-edit--literal new))
        result)
    (cl-flet ((names-in (array)
                (dolist (n (json-schema-edit-model-elements array))
                  (when (equal (json-schema-edit-model-string-value n) old)
                    (push (json-schema-edit-edit--node-replacement n new-lit) result))))
              (key-of (object)
                (when-let* ((member (json-schema-edit-model-get object old)))
                  (push (json-schema-edit-edit--key-replacement member new-lit) result))))
      (key-of (json-schema-edit-model-get owner 'properties))
      (names-in (json-schema-edit-model-get owner 'required))
      (dolist (kw '(dependentRequired dependencies))
        (let ((deps (json-schema-edit-model-get owner kw)))
          (key-of deps)
          (dolist (member (json-schema-edit-model-members deps))
            (names-in member))))
      ;; The values of `dependentSchemas' are schemas, not name arrays.
      (key-of (json-schema-edit-model-get owner 'dependentSchemas)))
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
         (owner (json-schema-edit-edit--node owner-ptr)))
    (cond
     ((equal old new))
     ((string-empty-p new) (user-error "Empty property name"))
     ((json-schema-edit-model-get (json-schema-edit-model-get owner 'properties) new)
      (user-error "Property %s already exists" new))
     (t
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit--apply-replacements
         (json-schema-edit-edit--rename-replacements owner old new)))))))

(defun json-schema-edit-edit--remove-name-references (owner-ptr name)
  "Remove references to the property NAME in the schema at OWNER-PTR.
Covers `required', `dependentRequired', `dependencies' and
`dependentSchemas'.  The owner is read again after every edit, since an
edit makes the nodes stale."
  (cl-flet ((owner () (json-schema-edit-edit--node owner-ptr)))
    (let ((required (json-schema-edit-model-get (owner) 'required)))
      (when (member name (json-schema-edit-edit--names required))
        (if (equal (json-schema-edit-edit--names required) (list name))
            (json-schema-edit-edit-remove-member (owner) 'required)
          (json-schema-edit-edit-remove-element
           required (json-schema-edit-edit--find-string required name)))))
    (dolist (kw '(dependentRequired dependencies dependentSchemas))
      ;; Remove NAME from the name arrays of KW.  One edit per
      ;; iteration, so the model is read again after every change.
      (let (deps v)
        (while (setq deps (json-schema-edit-model-get (owner) kw)
                     v (seq-find
                        (lambda (m)
                          (and (json-schema-edit-model-array-p m)
                               (member name (json-schema-edit-edit--names m))))
                        (json-schema-edit-model-members deps)))
          (if (equal (json-schema-edit-edit--names v) (list name))
              ;; A name array that became empty says nothing.
              (json-schema-edit-edit-remove-member
               deps (json-schema-edit-model-node-key v))
            (json-schema-edit-edit-remove-element
             v (json-schema-edit-edit--find-string v name)))))
      (json-schema-edit-edit-remove-member (json-schema-edit-model-get (owner) kw) name)
      ;; A keyword left with an empty object says nothing either.
      (let ((deps (json-schema-edit-model-get (owner) kw)))
        (when (and (json-schema-edit-model-object-p deps)
                   (null (json-schema-edit-model-members deps)))
          (json-schema-edit-edit-remove-member (owner) kw))))))

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
       (json-schema-edit-edit--node (append owner-ptr '(properties)))
       name)
      (json-schema-edit-edit--remove-name-references owner-ptr name))))

(defun json-schema-edit-edit--move-property (pointer delta)
  "Move the property at POINTER by DELTA (-1 or 1) within its object.
POINTER ends with \"properties\" and the property name; only the
properties of an object can be reordered.  Return POINTER."
  (unless (and (>= (length pointer) 2)
               (eq (json-schema-edit-model-keyword (car (last pointer 2))) 'properties))
    (user-error "Only the properties of an object can be moved"))
  (let* ((props (json-schema-edit-edit--node (append (butlast pointer 2) '(properties))))
         (members (json-schema-edit-model-members props))
         (member (json-schema-edit-model-get props (car (last pointer))))
         (other (and member
                     (+ (seq-position members member #'eq) delta))))
    (unless (and member (<= 0 other) (< other (length members)))
      (user-error "Cannot move the property further"))
    (let ((b (nth other members)))
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit--apply-replacements
         (list (list (json-schema-edit-model-member-beg member)
                     (json-schema-edit-model-node-end member)
                     (json-schema-edit-edit--member-text b))
               (list (json-schema-edit-model-member-beg b)
                     (json-schema-edit-model-node-end b)
                     (json-schema-edit-edit--member-text member))))))
    pointer))

(defun json-schema-edit-move-property-up (pointer)
  "Move the property at POINTER up among the properties of its object.
POINTER ends with \"properties\" and the property name."
  (interactive
   (list (or (json-schema-edit-edit--property-pointer)
             (user-error "Only the properties of an object can be moved"))))
  (json-schema-edit-edit--move-property pointer -1))

(defun json-schema-edit-move-property-down (pointer)
  "Move the property at POINTER down among the properties of its object.
POINTER ends with \"properties\" and the property name."
  (interactive
   (list (or (json-schema-edit-edit--property-pointer)
             (user-error "Only the properties of an object can be moved"))))
  (json-schema-edit-edit--move-property pointer 1))

;;;; Definitions

(defun json-schema-edit-edit--definition-pointer (&optional pos)
  "Return (KEYWORD NAME) of the definition at POS, or nil.
KEYWORD is a symbol.  On a local `$ref' the definition it points to is
used."
  (let* ((model (json-schema-edit-edit--model))
         (pos (or pos (point)))
         (segments (or (when-let* ((ref (json-schema-edit-model-ref-at model pos))
                                   (res (json-schema-edit-model-resolve
                                         model (json-schema-edit-model-ref-value ref))))
                         (unless (map-elt res :file)
                           (map-elt res :pointer)))
                       (json-schema-edit-model-pointer-at model pos))))
    (when-let* (((>= (length segments) 2))
                (keyword (car (memq (json-schema-edit-model-keyword (car segments))
                                    '($defs definitions)))))
      (list keyword (nth 1 segments)))))

(defun json-schema-edit-edit--same-file-p (a b)
  "Return non-nil if the file names A and B (possibly nil) are the same."
  (if (and a b) (file-equal-p a b) (eq a b)))

(defun json-schema-edit-edit--ref-rewrite (model value file prefix new-prefix)
  "Return VALUE rewritten from PREFIX to NEW-PREFIX, or nil.
VALUE is a `$ref' string of MODEL; it is rewritten when it resolves into
PREFIX of FILE (nil meaning MODEL itself) through a JSON Pointer."
  (pcase-let ((`(,resource . ,fragment) (json-schema-edit-model-split-ref value))
              (prefix (json-schema-edit-model-normalize-segments prefix))
              (self (json-schema-edit-model-file model)))
    (when (and fragment (string-prefix-p "/" fragment))
      (let ((segments (json-schema-edit-model-pointer-decode fragment))
            (res (json-schema-edit-model-resolve model value)))
        (when (and res
                   (json-schema-edit-edit--same-file-p
                    (or (map-elt res :file) self)
                    (or file self))
                   (equal (seq-take segments (length prefix)) prefix)
                   (equal (seq-take (json-schema-edit-model-normalize-segments
                                     (map-elt res :pointer))
                                    (length prefix))
                          prefix))
          (concat resource
                  (json-schema-edit-model-pointer-fragment
                   (append new-prefix (seq-drop segments (length prefix))))))))))

(defun json-schema-edit-edit--ref-replacements (model file prefix new-prefix)
  "Return replacements for the `$ref's of MODEL into PREFIX of FILE."
  (delq nil
        (mapcar (lambda (ref)
                  (when-let* ((new (json-schema-edit-edit--ref-rewrite
                                    model (json-schema-edit-model-ref-value ref)
                                    file prefix new-prefix)))
                    (list (json-schema-edit-model-ref-beg ref)
                          (json-schema-edit-model-ref-end ref)
                          (json-schema-edit-edit--literal new))))
                (json-schema-edit-model-refs model))))

(defun json-schema-edit-edit--other-files (prefix)
  "Return the other schema files with `$ref's into PREFIX of this file."
  (when-let* ((self buffer-file-name))
    (seq-filter
     (lambda (file)
       (and (not (file-equal-p file self))
            (when-let* ((model (json-schema-edit-model-of-file file)))
              (json-schema-edit-edit--ref-replacements model self prefix prefix))))
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
KEYWORD is a symbol or a string.
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
               (model (json-schema-edit-edit--model))
               (defs (json-schema-edit-model-get (json-schema-edit-model-root model) keyword)))
    (cond
     ((equal old new))
     ((string-empty-p new) (user-error "Empty definition name"))
     ((json-schema-edit-model-get defs new)
      (user-error "Definition %s already exists" new))
     (t
      (let* ((self buffer-file-name)
             (others (json-schema-edit-edit--other-files prefix))
             (def (json-schema-edit-model-get defs old))
             ;; Collected before this buffer changes, since resolving
             ;; them needs the old name.
             (external
              (when (and others
                         (y-or-n-p (format "Update references in %s? "
                                           (json-schema-edit-edit--file-list-text others))))
                (mapcar (lambda (file)
                          (with-current-buffer (find-file-noselect file)
                            (json-schema-edit-model-parser)
                            (cons (current-buffer)
                                  (json-schema-edit-edit--ref-replacements
                                   (json-schema-edit-model-current)
                                   self prefix new-prefix))))
                        others))))
        (json-schema-edit-edit--with-undo
          (json-schema-edit-edit--apply-replacements
           (cons (json-schema-edit-edit--key-replacement
                  def (json-schema-edit-edit--literal new))
                 (json-schema-edit-edit--ref-replacements model nil prefix new-prefix))))
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
