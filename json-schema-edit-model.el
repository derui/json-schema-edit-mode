;;; json-schema-edit-model.el --- Schema model built from the json tree-sitter tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The schema model is the single place that interprets a JSON Schema
;; buffer.  Every UI feature (xref, eldoc, breadcrumb, ...) asks this
;; module instead of walking the syntax tree by itself, so that all of
;; them share the same interpretation.
;;
;; A model is plain data: a tree of `json-schema-edit-model-node' built
;; once from the buffer, with an index of `$ref', `$anchor' and `$id'.
;; Every query takes a model or a node and never looks at a buffer;
;; only the functions under "Building models" read buffers, to build a
;; model.  `json-schema-edit-model-current' returns the model of the
;; current buffer, rebuilt when the buffer has changed.  Nodes keep the
;; positions they were read from, so that editing code can write back to
;; the buffer; after an edit, ask for the model again.
;;
;; Locations inside a schema are expressed as JSON Pointers (RFC 6901).
;; Internally a pointer is a list of segments: symbols for keywords,
;; strings for names (of properties, definitions, ...) and other object
;; keys, and integers for array indices, such as (properties "name").
;;
;; Schema keywords are handled as symbols (`properties', `$ref', ...)
;; everywhere: the keys of the members of a schema object are symbols,
;; while property names and other data from the buffer stay strings.
;; Pointers and keywords from elsewhere, such as user input or a `$ref'
;; fragment, are brought to that form by
;; `json-schema-edit-model-canonical-pointer' and
;; `json-schema-edit-model-keyword'.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(declare-function treesit-node-child-by-field-name "treesit.c") (defgroup json-schema-edit nil
  "Editing support for JSON Schema."
  :group 'languages
  :prefix "json-schema-edit-")

(defcustom json-schema-edit-schema-versions
  '(("https://json-schema.org/draft/2020-12/schema" . 2020-12)
    ("https://json-schema.org/draft/2019-09/schema" . 2019-09)
    ("https://json-schema.org/draft-07/schema#" . draft-07)
    ("https://json-schema.org/draft-07/schema" . draft-07)
    ("https://json-schema.org/draft-06/schema#" . draft-06)
    ("https://json-schema.org/draft-06/schema" . draft-06)
    ("https://json-schema.org/draft-04/schema#" . draft-04)
    ("https://json-schema.org/draft-04/schema" . draft-04))
  "Alist mapping `$schema' URIs to schema versions.
Each key is compared with the whole value of `$schema', so a URI must
be listed exactly as it is written in the schema.  Each value is one of
`2020-12', `2019-09', `draft-07', `draft-06' or `draft-04'."
  :type '(alist :key-type
                (string :tag "URI")
                :value-type (choice
                             (const 2020-12)
                             (const 2019-09)
                             (const draft-07)
                             (const draft-06)
                             (const draft-04)))
  :group 'json-schema-edit)

(defconst json-schema-edit-model--keyword-types
  '((minLength . string)
    (maxLength . string)
    (pattern . string)
    (format . string)
    (contentEncoding . string)
    (contentMediaType . string)
    (minimum . number)
    (maximum . number)
    (exclusiveMinimum . number)
    (exclusiveMaximum . number)
    (multipleOf . number)
    (items . array)
    (prefixItems . array)
    (additionalItems . array)
    (minItems . array)
    (maxItems . array)
    (uniqueItems . array)
    (contains . array)
    (minContains . array)
    (maxContains . array)
    (unevaluatedItems . array)
    (properties . object)
    (patternProperties . object)
    (additionalProperties . object)
    (required . object)
    (minProperties . object)
    (maxProperties . object)
    (propertyNames . object)
    (dependentRequired . object)
    (dependentSchemas . object)
    (dependencies . object)
    (unevaluatedProperties . object))
  "Keywords that apply only to one type.")

(defconst json-schema-edit-model--combinators
  '(oneOf anyOf allOf)
  "Keywords whose array elements are alternative subschemas.")

;;;; Structures

(cl-defstruct
    (json-schema-edit-model
     (:constructor json-schema-edit-model--create)
     (:copier nil))
  "A JSON document read as a schema."
  root       ; top-level `json-schema-edit-model-node', or nil
  source     ; the whole text the model was built from
  file       ; file name of the document, or nil
  buffer     ; buffer the model was built from, or nil
  directory  ; directory relative references are resolved against
  refs       ; `json-schema-edit-model-ref' of every $ref
  anchors    ; alist of ($anchor or $dynamicAnchor value . object node)
  ids)       ; alist of ($id value . object node)

(cl-defstruct
    (json-schema-edit-model-node
     (:constructor json-schema-edit-model--node-create)
     (:copier nil))
  "A JSON value of a model."
  type       ; object, array, string, number, true, false, null, ...
  value      ; member nodes, element nodes, decoded string or number
  key        ; key in the parent object (a symbol for a keyword, else a
             ; string), index in the parent array, or nil
  key-beg    ; region of the key literal for an object member, else nil
  key-end
  pointer    ; segments from the root
  beg end    ; region of the value
  source)    ; text of the whole document; see `json-schema-edit-model-node-text'

(cl-defstruct
    (json-schema-edit-model-ref
     (:constructor json-schema-edit-model-ref-create)
     (:copier nil))
  "A $ref occurrence."
  value     ; the $ref string as written
  pointer   ; segments of the schema object that holds the $ref
  beg end)  ; region of the $ref string literal

;;;; Nodes

(defun json-schema-edit-model-json-type (node)
  "Return the JSON type of NODE as a symbol such as `object', or nil."
  (and node (json-schema-edit-model-node-type node)))

(defun json-schema-edit-model-object-p (node)
  "Return non-nil if NODE is an object."
  (eq (json-schema-edit-model-json-type node) 'object))

(defun json-schema-edit-model-array-p (node)
  "Return non-nil if NODE is an array."
  (eq (json-schema-edit-model-json-type node) 'array))

(defun json-schema-edit-model-members (node)
  "Return the member nodes of the object NODE, in order.
The key of a member is `json-schema-edit-model-node-key'."
  (when (json-schema-edit-model-object-p node)
    (json-schema-edit-model-node-value node)))

(defun json-schema-edit-model-elements (node)
  "Return the element nodes of the array NODE, in order."
  (when (json-schema-edit-model-array-p node)
    (json-schema-edit-model-node-value node)))

(defun json-schema-edit-model-children (node)
  "Return the members or elements of NODE."
  (when (memq (json-schema-edit-model-json-type node) '(object array))
    (json-schema-edit-model-node-value node)))

(defun json-schema-edit-model-member-beg (node)
  "Return where NODE starts with its key: the key of a member, else NODE."
  (or
   (json-schema-edit-model-node-key-beg node)
   (json-schema-edit-model-node-beg node)))

(defun json-schema-edit-model-node-text (node)
  "Return the JSON text of NODE as written."
  (substring
   (json-schema-edit-model-node-source node)
   (1- (json-schema-edit-model-node-beg node))
   (1- (json-schema-edit-model-node-end node))))

(defun json-schema-edit-model-string-value (node)
  "Return the decoded string of the string NODE, or nil."
  (when (eq (json-schema-edit-model-json-type node) 'string)
    (json-schema-edit-model-node-value node)))

(defun json-schema-edit-model-descendants (node)
  "Return NODE and every node below it, in document order."
  (when node
    (cons node
          (mapcan #'json-schema-edit-model-descendants
                  (json-schema-edit-model-children node)))))

(defun json-schema-edit-model-key-name (key)
  "Return KEY as a string.
KEY is a keyword symbol such as `properties', a string such as a
property name, or an integer array index."
  (cond
   ((symbolp key)
    (symbol-name key))
   ((integerp key)
    (number-to-string key))
   (t key)))

(defun json-schema-edit-model-keyword (key)
  "Return the keyword symbol named by KEY, or nil.
KEY is a symbol, returned as is, or a string such as user input or a
pointer segment.  Integers give nil."
  (cond
   ((symbolp key)
    key)
   ((stringp key)
    (intern key))))

(defun json-schema-edit-model-get (node key)
  "Return the value node for KEY in the object NODE, or nil.
KEY is a keyword symbol or a string, matched by its name, so that a
segment decoded from a JSON Pointer finds a keyword too."
  (let ((key (json-schema-edit-model-key-name key)))
    (seq-find
     (lambda (m)
       (equal
        (json-schema-edit-model-key-name (json-schema-edit-model-node-key m))
        key))
     (json-schema-edit-model-members node))))

(defun json-schema-edit-model-get-string (node key)
  "Return the string value for KEY in the object NODE, or nil."
  (json-schema-edit-model-string-value
   (json-schema-edit-model-get node key)))

;;;; JSON Pointer

(defun json-schema-edit-model-pointer-encode (segments)
  "Encode SEGMENTS as a JSON Pointer string such as \"/a/0\"."
  (mapconcat
   (lambda (s)
     (concat "/"
             (string-replace "/" "~1"
                             (string-replace "~" "~0"
                                             (json-schema-edit-model-key-name s)))))
   segments ""))

(defun json-schema-edit-model-pointer-fragment (segments)
  "Encode SEGMENTS as a URI fragment such as \"#/a/0\"."
  (concat "#" (json-schema-edit-model-pointer-encode segments)))

(defun json-schema-edit-model-pointer-decode (pointer)
  "Decode the JSON Pointer string POINTER into a list of segments.
Percent-encoding from a URI fragment is undone as well.  Segments that
look like array indices are kept as strings; lookup decides how to
interpret them."
  (let ((p (decode-coding-string (url-unhex-string pointer) 'utf-8)))
    (if (string-empty-p p)
        nil
      (unless (string-prefix-p "/" p)
        (error "Invalid JSON Pointer: %s" pointer))
      (mapcar
       (lambda (s)
         (string-replace "~0" "~" (string-replace "~1" "/" s)))
       (cdr (split-string p "/"))))))

(defun json-schema-edit-model--index-segment (seg)
  "Return SEG as an array index when it is one, else nil."
  (cond
   ((integerp seg)
    seg)
   ((and
     (stringp seg)
     (string-match-p "\\`[0-9]+\\'" seg))
    (string-to-number seg))))

(defun json-schema-edit-model-canonical-pointer (segments)
  "Return SEGMENTS in the form the model uses for pointers.
Keywords become symbols, names and other keys strings, and array
indices of subschemas integers, so that a pointer from user input or a
`$ref' fragment compares with `equal' to the pointers of the model."
  (let ((state 'schema)
        result)
    (dolist (seg segments)
      (let ((seg
             (pcase state
               ((or 'schema 'schema-or-array)
                (or
                 (and
                  (eq state 'schema-or-array)
                  (json-schema-edit-model--index-segment seg))
                 (and (not (integerp seg))
                      (json-schema-edit-model-keyword seg))
                 seg))
               ('array
                (or (json-schema-edit-model--index-segment seg) seg))
               (_
                (if (integerp seg)
                    seg
                  (json-schema-edit-model-key-name seg))))))
        (push seg result)
        (setq state (json-schema-edit-model--next-state state seg))))
    (nreverse result)))

(defun json-schema-edit-model-lookup (node segments)
  "Return the node reached by SEGMENTS from NODE, or nil."
  (while (and node segments)
    (let ((seg (pop segments)))
      (setq node
            (pcase (json-schema-edit-model-json-type node)
              ('object
               (json-schema-edit-model-get node seg))
              ('array
               (let ((idx (json-schema-edit-model--index-segment seg)))
                 (and idx
                      (nth idx
                           (json-schema-edit-model-elements node)))))))))
  node)

(defun json-schema-edit-model-node-at-pointer (model segments)
  "Return the node of MODEL reached by SEGMENTS from its root."
  (json-schema-edit-model-lookup
   (json-schema-edit-model-root model)
   segments))

;;;; Positions

(defun json-schema-edit-model--skip-whitespace (model pos)
  "Return the position of the first non-whitespace character from POS in MODEL."
  (let ((i
         (string-match-p "[^ \t\n\r]"
                         (json-schema-edit-model-source model)
                         (max 0 (1- pos)))))
    (if i (1+ i) pos)))

(defun json-schema-edit-model-node-at (model pos)
  "Return the innermost node of MODEL at POS, or nil.
A member is entered from its key.  In whitespace, the node starting
after it is taken."
  (let* ((pos (json-schema-edit-model--skip-whitespace model pos))
         (contains
          (lambda (n)
            (and
             (<= (json-schema-edit-model-member-beg n) pos)
             (< pos (json-schema-edit-model-node-end n)))))
         (node (json-schema-edit-model-root model))
         found)
    (when (and node (funcall contains node))
      (while node
        (setq found node
              node
              (seq-find contains
                        (json-schema-edit-model-children node)))))
    found))

(defun json-schema-edit-model-pointer-at (model pos)
  "Return the pointer segments of the value of MODEL containing POS.
When POS is on an object key, the pointer of that member's value is
returned."
  (when-let* ((node (json-schema-edit-model-node-at model pos)))
    (json-schema-edit-model-node-pointer node)))

(defun json-schema-edit-model-line-column (model pos)
  "Return (LINE . COLUMN) of POS in the source of MODEL.
LINE counts from 1; COLUMN is the display width before POS on its line."
  (let* ((source (json-schema-edit-model-source model))
         (end
          (min (max 0 (1- pos)) (length source)))
         (bol
          (let ((i (cl-position ?\n source :end end :from-end t)))
            (if i (1+ i) 0))))
    (cons
     (1+ (cl-count ?\n source :end end))
     (string-width (substring source bol end)))))

(defun json-schema-edit-model-line-string (model pos)
  "Return the trimmed text of the line holding POS in the source of MODEL."
  (let* ((source (json-schema-edit-model-source model))
         (i
          (min (max 0 (1- pos)) (length source)))
         (bol
          (let ((j (cl-position ?\n source :end i :from-end t)))
            (if j (1+ j) 0)))
         (eol (or (cl-position ?\n source :start i) (length source))))
    (string-trim (substring source bol eol))))

;;;; Schema positions

(defun json-schema-edit-model-schema-pointer-at (model pos)
  "Return the pointer of the innermost schema object of MODEL at POS.
Positions inside keyword values that are not schemas (such as the
string of `type') are mapped to the object that holds them."
  (let ((segments (json-schema-edit-model-pointer-at model pos)))
    (while (and segments
                (not (and
                      (json-schema-edit-model-schema-position-p segments)
                      (json-schema-edit-model-object-p
                       (json-schema-edit-model-node-at-pointer model segments)))))
      (setq segments (butlast segments)))
    segments))

(defconst json-schema-edit-model--map-keywords
  '(properties patternProperties $defs definitions dependentSchemas
               dependencies)
  "Keywords whose value maps names to subschemas.")

(defconst json-schema-edit-model--schema-keywords
  '(additionalItems additionalProperties contains not if then else
                    propertyNames unevaluatedItems unevaluatedProperties
                    contentSchema)
  "Keywords whose value is a subschema.")

(defconst json-schema-edit-model--array-keywords
  '(oneOf anyOf allOf prefixItems)
  "Keywords whose value is an array of subschemas.")

(defun json-schema-edit-model--next-state (state seg)
  "Return the state reached from STATE through the segment SEG.
A state tells what a value is: `schema', `schema-or-array' (the value
of `items'), `map' (names to subschemas), `array' (of subschemas) or
`data'."
  (pcase state
    ((or 'schema 'schema-or-array)
     (let ((keyword
            (and (not (integerp seg))
                 (json-schema-edit-model-keyword seg))))
       (cond
        ((and (eq state 'schema-or-array) (integerp seg))
         'schema)
        ((memq keyword json-schema-edit-model--map-keywords)
         'map)
        ((eq keyword 'items)
         'schema-or-array)
        ((memq keyword json-schema-edit-model--schema-keywords)
         'schema)
        ((memq keyword json-schema-edit-model--array-keywords)
         'array)
        (t 'data))))
    ('map 'schema)
    ('array
     (if (integerp seg) 'schema 'data))
    (_ 'data)))

(defun json-schema-edit-model--schema-state-p (state)
  "Return non-nil if a value in STATE is a schema, whose keys are keywords."
  (memq state '(schema schema-or-array)))

(defun json-schema-edit-model-schema-position-p (segments)
  "Return non-nil if SEGMENTS address a subschema rather than a keyword value.
The whole path is followed from the root, so that objects inside
`examples', `default' or `enum' are not taken for schemas."
  (let ((state 'schema))
    (dolist (seg segments)
      (setq state (json-schema-edit-model--next-state state seg)))
    (json-schema-edit-model--schema-state-p state)))

(defun json-schema-edit-model-schemas (model)
  "Return every schema object of MODEL, in document order."
  (seq-filter
   (lambda (n)
     (and
      (json-schema-edit-model-object-p n)
      (json-schema-edit-model-schema-position-p
       (json-schema-edit-model-node-pointer n))))
   (json-schema-edit-model-descendants
    (json-schema-edit-model-root model))))

;;;; Index of $ref / $anchor / $id

(defun json-schema-edit-model--index (root)
  "Return the index of the tree ROOT as a plist (:refs :anchors :ids)."
  (let (refs anchors dynamic-anchors ids)
    (dolist (object (json-schema-edit-model-descendants root))
      (dolist (m (json-schema-edit-model-members object))
        (when-let* ((value (json-schema-edit-model-string-value m)))
          (pcase (json-schema-edit-model-node-key m)
            ('$ref
             (push
              (json-schema-edit-model-ref-create
               :value value
               :pointer (json-schema-edit-model-node-pointer object)
               :beg (json-schema-edit-model-node-beg m)
               :end (json-schema-edit-model-node-end m))
              refs))
            ('$anchor (push (cons value object) anchors))
            ('$dynamicAnchor
             (push (cons value object) dynamic-anchors))
            ('$id (push (cons value object) ids))))))
    (list :refs
          (nreverse refs)
          :anchors (nconc (nreverse anchors) (nreverse dynamic-anchors))
          :ids (nreverse ids))))

(defun json-schema-edit-model-make (root source &optional file buffer directory)
  "Return a model of the tree ROOT read from SOURCE.
FILE and BUFFER are where SOURCE came from, either possibly nil;
DIRECTORY is where relative references are resolved from."
  (let ((index (json-schema-edit-model--index root)))
    (json-schema-edit-model--create
     :root root :source source :file file :buffer buffer
     :directory (or directory (and file (file-name-directory file)))
     :refs (map-elt index :refs)
     :anchors (map-elt index :anchors)
     :ids (map-elt index :ids))))

(defun json-schema-edit-model-ref-at (model pos)
  "Return the `json-schema-edit-model-ref' of MODEL whose literal contains POS."
  (seq-find
   (lambda (r)
     (and
      (<= (json-schema-edit-model-ref-beg r) pos)
      (< pos (json-schema-edit-model-ref-end r))))
   (json-schema-edit-model-refs model)))

;;;; Draft

(defun json-schema-edit-model-schema-version (model)
  "Return the version of the schema MODEL as a symbol.
The whole `$schema' URI is looked up in
`json-schema-edit-schema-versions'.  Return nil when `$schema' is
missing or not listed there."
  (when-let* ((uri
               (json-schema-edit-model-get-string
                (json-schema-edit-model-root model)
                '$schema)))
    (cdr (assoc uri json-schema-edit-schema-versions))))

(defun json-schema-edit-model-schema-p (model)
  "Return non-nil if MODEL looks like a JSON Schema."
  (when-let* ((root (json-schema-edit-model-root model)))
    (or
     (json-schema-edit-model-get root '$schema)
     (json-schema-edit-model-get root '$defs)
     (json-schema-edit-model-get root 'definitions)
     (seq-some
      (lambda (keyword) (json-schema-edit-model-get root keyword))
      json-schema-edit-model--combinators))))

(defun json-schema-edit-model-defs-keyword (model)
  "Return the keyword holding definitions in MODEL, as a symbol.
An existing `$defs' or `definitions' wins; otherwise it follows the
schema version: `definitions' up to draft-07, `$defs' after."
  (let ((root (json-schema-edit-model-root model)))
    (cond
     ((json-schema-edit-model-get root '$defs)
      '$defs)
     ((json-schema-edit-model-get root 'definitions)
      'definitions)
     ((memq
       (json-schema-edit-model-schema-version model)
       '(draft-04 draft-06 draft-07))
      'definitions)
     (t '$defs))))

(defun json-schema-edit-model-ref-siblings-ignored-p (model)
  "Return non-nil if keywords next to `$ref' are ignored by the version of MODEL."
  (memq
   (json-schema-edit-model-schema-version model)
   '(draft-04 draft-06 draft-07)))

;;;; Resolving $ref

(defun json-schema-edit-model-split-ref (ref)
  "Split REF into (RESOURCE . FRAGMENT).
RESOURCE is the part before `#' (possibly empty); FRAGMENT is the part
after it or nil when there is no `#'."
  (if-let* ((i (string-search "#" ref)))
      (cons (substring ref 0 i) (substring ref (1+ i)))
    (cons ref nil)))

(defun json-schema-edit-model-remote-p (resource)
  "Return non-nil if RESOURCE is a URI with a scheme other than file."
  (and
   (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" resource)
   (not (equal (downcase (match-string 1 resource)) "file"))))

(defun json-schema-edit-model-resource-file (model resource)
  "Return the absolute file name of RESOURCE, relative to MODEL."
  (let ((path
         (url-unhex-string (string-remove-prefix "file://" resource))))
    (expand-file-name path (json-schema-edit-model-directory model))))

(defun json-schema-edit-model-resolve-fragment (model fragment)
  "Return the node of MODEL that FRAGMENT names, or nil.
FRAGMENT is a JSON Pointer, an anchor, or nil or empty for the root."
  (cond
   ((or (null fragment) (string-empty-p fragment))
    (json-schema-edit-model-root model))
   ((string-prefix-p "/" fragment)
    (json-schema-edit-model-node-at-pointer
     model
     (json-schema-edit-model-pointer-decode fragment)))
   (t
    (cdr
     (assoc
      (url-unhex-string fragment)
      (json-schema-edit-model-anchors model))))))

(defun json-schema-edit-model--id-match-p (id resource)
  "Return non-nil if the `$id' ID names RESOURCE.
Either they are equal, or one is a path suffix of the other, which
covers relative references against an absolute `$id'."
  (let ((id (string-remove-suffix "#" id)))
    (or
     (equal id resource)
     (string-suffix-p (concat "/" resource) id)
     (string-suffix-p (concat "/" id) resource))))

(defun json-schema-edit-model--target (file model node)
  "Return the resolution plist of NODE in MODEL, which is FILE or nil."
  (when node
    (list :file file :model model :node node
          :pos (json-schema-edit-model-node-beg node)
          :pointer (json-schema-edit-model-node-pointer node))))

(defun json-schema-edit-model-resolve (model ref &optional loader)
  "Resolve REF from MODEL.
Return a plist (:file FILE :model MODEL :node NODE :pos POS :pointer
SEGMENTS) where FILE is nil when the target is in MODEL itself, or nil
when REF cannot be resolved.  Remote references (http etc.) are not
resolved.  LOADER is called with an absolute file name and returns its
model or nil; it defaults to `json-schema-edit-model-of-file'."
  (pcase-let* ((`(,resource . ,fragment)
                (json-schema-edit-model-split-ref ref)))
    (cond
     ((string-empty-p resource)
      (json-schema-edit-model--target
       nil model
       (json-schema-edit-model-resolve-fragment model fragment)))
     ;; A resource embedded in this document by `$id'.
     ((when-let* ((id
                   (seq-find
                    (lambda (e)
                      (json-schema-edit-model--id-match-p (car e) resource))
                    (json-schema-edit-model-ids model))))
        (json-schema-edit-model--target
         nil model
         (if (and fragment (string-prefix-p "/" fragment))
             (json-schema-edit-model-lookup
              (cdr id)
              (json-schema-edit-model-pointer-decode fragment))
           (cdr id)))))
     ((json-schema-edit-model-remote-p resource)
      nil)
     (t
      (let* ((file
              (json-schema-edit-model-resource-file model resource))
             (other
              (funcall
               (or loader #'json-schema-edit-model-of-file)
               file)))
        (when other
          (json-schema-edit-model--target
           file other
           (json-schema-edit-model-resolve-fragment other fragment))))))))

;;;; Summaries

(defun json-schema-edit-model-root-name (model)
  "Return a name for the root schema of MODEL: its title, or the file's base name."
  (or
   (json-schema-edit-model-get-string
    (json-schema-edit-model-root model)
    'title)
   (when-let* ((file (json-schema-edit-model-file model)))
     (file-name-base
      (string-remove-suffix ".schema" (file-name-base file))))
   "root"))

(defun json-schema-edit-model-declared-types (node)
  "Return the types declared by `type' of the schema NODE, or nil."
  (when-let* ((type (json-schema-edit-model-get node 'type)))
    (pcase (json-schema-edit-model-json-type type)
      ('string
       (list (json-schema-edit-model-string-value type)))
      ('array
       (delq nil
             (mapcar #'json-schema-edit-model-string-value
                     (json-schema-edit-model-elements type)))))))

(defun json-schema-edit-model-keyword-applies-p (keyword types)
  "Return non-nil if KEYWORD has an effect on a value of one of TYPES.
KEYWORD is a symbol or a string.  Keywords not tied to a type always
apply."
  (let ((applies
         (alist-get
          (json-schema-edit-model-keyword keyword)
          json-schema-edit-model--keyword-types)))
    (or
     (null applies)
     (member
      (symbol-name applies)
      (if (member "integer" types) (cons "number" types) types)))))

(defun json-schema-edit-model-type-string (node)
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

(defun json-schema-edit-model-summary (node)
  "Return a one-line summary (type and description) of the schema NODE."
  (when (json-schema-edit-model-object-p node)
    (let ((type (json-schema-edit-model-type-string node))
          (desc
           (or
            (json-schema-edit-model-get-string node 'title)
            (json-schema-edit-model-get-string node 'description))))
      (string-join (delq nil (list type desc)) " — "))))

;;;; Building models

;; Only the functions below read buffers.

(defun json-schema-edit-model-parser ()
  "Return the json parser of the current buffer, creating one if needed."
  (or
   (car (treesit-parser-list nil 'json))
   (treesit-parser-create 'json)))

(defun json-schema-edit-model--ts-children (node)
  "Return the named children of the tree-sitter NODE except comments."
  (seq-remove
   (lambda (n) (equal (treesit-node-type n) "comment"))
   (treesit-node-children node t)))

(defun json-schema-edit-model--ts-key (key)
  "Return the decoded string of the tree-sitter KEY node of a pair."
  (let ((text (treesit-node-text key t)))
    (if (and
         (equal (treesit-node-type key) "string")
         (not (string-search "\\" text)))
        (substring text 1 -1)
      (condition-case nil (json-parse-string text) (error nil)))))

(defun json-schema-edit-model--convert (ts pointer state source &optional key key-node)
  "Return the model node of the tree-sitter node TS at POINTER.
STATE is what TS is, as for `json-schema-edit-model--next-state'; the
keys of a schema object become keyword symbols.  SOURCE is the text of
the document.  KEY is the key or index of TS in its parent, and
KEY-NODE the tree-sitter key of a member."
  (let ((type (intern (treesit-node-type ts))))
    (json-schema-edit-model--node-create
     :type type
     :value (pcase type
              ('object
               (let (members)
                 (dolist (pair (treesit-node-children ts t))
                   (when-let* (((equal (treesit-node-type pair) "pair"))
                               (k (treesit-node-child-by-field-name pair "key"))
                               (v (treesit-node-child-by-field-name pair "value")))
                     (let* ((name (json-schema-edit-model--ts-key k))
                            (name
                             (if (and name
                                      (json-schema-edit-model--schema-state-p state))
                                 (intern name)
                               name)))
                       (push
                        (json-schema-edit-model--convert
                         v
                         (append pointer (list name))
                         (json-schema-edit-model--next-state state name)
                         source name k)
                        members))))
                 (nreverse members)))
              ('array
               (let ((i 0)
                     elements)
                 (dolist (child (json-schema-edit-model--ts-children ts))
                   (push
                    (json-schema-edit-model--convert
                     child
                     (append pointer (list i))
                     (json-schema-edit-model--next-state state i)
                     source i)
                    elements)
                   (setq i (1+ i)))
                 (nreverse elements)))
              ('string
               (condition-case nil
                   (json-parse-string (treesit-node-text ts t))
                 (error nil)))
              ('number
               (string-to-number (treesit-node-text ts t))))
     :key key
     :key-beg (and key-node (treesit-node-start key-node))
     :key-end (and key-node (treesit-node-end key-node))
     :pointer pointer
     :beg (treesit-node-start ts)
     :end (treesit-node-end ts)
     :source source)))

(defun json-schema-edit-model--build (file buffer directory)
  "Return the model of the current buffer, recorded as FILE, BUFFER and DIRECTORY."
  (save-restriction
    (widen)
    (let* ((source
            (buffer-substring-no-properties (point-min) (point-max)))
           (top
            (seq-find
             (lambda (n)
               (not (equal (treesit-node-type n) "comment")))
             (treesit-node-children
              (treesit-parser-root-node
               (json-schema-edit-model-parser))
              t))))
      (json-schema-edit-model-make
       (and top (json-schema-edit-model--convert top nil 'schema source))
       source file buffer directory))))

(defvar-local json-schema-edit-model--current nil
  "The model of the buffer as (TICK . MODEL).")

(defun json-schema-edit-model-current ()
  "Return the model of the current buffer, rebuilding it when stale."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql (car json-schema-edit-model--current) tick)
      (setq json-schema-edit-model--current
            (cons tick
                  (json-schema-edit-model--build
                   buffer-file-name
                   (current-buffer)
                   (if buffer-file-name
                       (file-name-directory buffer-file-name)
                     default-directory)))))
    (cdr json-schema-edit-model--current)))

(defvar json-schema-edit-model--file-cache
  (make-hash-table :test #'equal)
  "File name to (MTIME . MODEL) of the files read from disk.")

(defun json-schema-edit-model-of-file (file)
  "Return the model of FILE, or nil when it cannot be read.
A live buffer visiting FILE is used when it has a json parser, so that
unsaved edits are seen; otherwise FILE is read, and the model is kept
until FILE changes."
  (let ((buf (find-buffer-visiting file)))
    (if (and buf
             (with-current-buffer buf
               (treesit-parser-list nil 'json)))
        (with-current-buffer buf (json-schema-edit-model-current))
      (when (file-readable-p file)
        (let ((mtime
               (file-attribute-modification-time
                (file-attributes file)))
              (cached
               (gethash file json-schema-edit-model--file-cache)))
          (if (equal (car cached) mtime)
              (cdr cached)
            (let ((model
                   (with-temp-buffer
                     (insert-file-contents file)
                     (json-schema-edit-model--build file nil nil))))
              (puthash file
                       (cons mtime model)
                       json-schema-edit-model--file-cache)
              model)))))))

(defun json-schema-edit-model-from-string (string)
  "Return the model of the JSON text STRING."
  (with-temp-buffer
    (insert string)
    (json-schema-edit-model--build nil nil default-directory)))

(provide 'json-schema-edit-model)
;;; json-schema-edit-model.el ends here
