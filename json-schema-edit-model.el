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
;; Locations inside a schema are expressed as JSON Pointers (RFC 6901).
;; Internally a pointer is a list of segments: strings for object keys
;; and integers for array indices.  Pointers built by code may also use
;; symbols for keywords, such as (properties "name"); compare pointers
;; after `json-schema-edit-model-normalize-segments'.
;;
;; Schema keywords are handled as symbols (`properties', `$ref', ...),
;; while property names and other data from the buffer stay strings.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(declare-function treesit-node-child-by-field-name "treesit.c")

(defgroup json-schema-edit nil
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
  :type '(alist :key-type (string :tag "URI")
                :value-type (choice (const 2020-12)
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

;;;; Tree helpers

(defun json-schema-edit-model--parser ()
  "Return the json parser of the current buffer, creating one if needed."
  (or
   (car (treesit-parser-list nil 'json))
   (treesit-parser-create 'json)))

(defun json-schema-edit-model-root ()
  "Return the top-level value node of the current buffer, or nil."
  (let ((doc
         (treesit-parser-root-node (json-schema-edit-model--parser))))
    (seq-find
     (lambda (n)
       (not (equal (treesit-node-type n) "comment")))
     (treesit-node-children doc t))))

(defun json-schema-edit-model--value-children (node)
  "Return the named children of NODE except comments; nil for no NODE."
  (when node
    (seq-remove
     (lambda (n) (equal (treesit-node-type n) "comment"))
     (treesit-node-children node t))))

(defun json-schema-edit-model-string-value (node)
  "Return the decoded string of the string NODE, or nil."
  (when (and node (equal (treesit-node-type node) "string"))
    (condition-case nil
        (json-parse-string (treesit-node-text node t))
      (error nil))))

(defun json-schema-edit-model-key-name (key)
  "Return KEY as a string.
KEY is a keyword symbol such as `properties', a string such as a
property name, or an integer array index."
  (cond
   ((symbolp key) (symbol-name key))
   ((integerp key) (number-to-string key))
   (t key)))

(defun json-schema-edit-model-keyword (key)
  "Return the keyword symbol named by KEY, or nil.
KEY is a symbol, returned as is, or a string such as an object key or a
pointer segment.  Integers give nil."
  (cond
   ((symbolp key) key)
   ((stringp key) (intern-soft key))))

(defun json-schema-edit-model-pair-key (pair)
  "Return the decoded key of PAIR."
  (let* ((key (treesit-node-child-by-field-name pair "key"))
         (text (treesit-node-text key t)))
    (if (and
         (equal (treesit-node-type key) "string")
         (not (string-search "\\" text)))
        (substring text 1 -1)
      (json-schema-edit-model-string-value key))))

(defun json-schema-edit-model-pair-value (pair)
  "Return the value node of PAIR."
  (treesit-node-child-by-field-name pair "value"))

(defun json-schema-edit-model-object-pairs (node)
  "Return the pair nodes of the object NODE."
  (when (and node (equal (treesit-node-type node) "object"))
    (seq-filter
     (lambda (n) (equal (treesit-node-type n) "pair"))
     (treesit-node-children node t))))

(defvar json-schema-edit-model--members-cache nil
  "Hash table from (BUFFER . START) of an object to its members, or nil.
Bound by `json-schema-edit-model-with-cache'.")

(defmacro json-schema-edit-model-with-cache (&rest body)
  "Evaluate BODY with member lookups of objects cached.
BODY must not modify the buffers it reads."
  (declare (indent 0) (debug t))
  `(let ((json-schema-edit-model--members-cache
          (or json-schema-edit-model--members-cache
              (make-hash-table :test #'equal))))
     ,@body))

(defun json-schema-edit-model--members (node)
  "Return an alist of (KEY . VALUE-NODE) for the object NODE."
  (mapcar
   (lambda (p)
     (cons
      (json-schema-edit-model-pair-key p)
      (json-schema-edit-model-pair-value p)))
   (json-schema-edit-model-object-pairs node)))

(defun json-schema-edit-model-get (node key)
  "Return the value node for KEY in the object NODE, or nil.
KEY is a keyword symbol or a string; see
`json-schema-edit-model-key-name'."
  (when (and node (equal (treesit-node-type node) "object"))
    (setq key (json-schema-edit-model-key-name key))
    (if json-schema-edit-model--members-cache
        (let* ((id (cons (current-buffer) (treesit-node-start node)))
               (members
                (or
                 (gethash id json-schema-edit-model--members-cache)
                 (puthash id
                          (json-schema-edit-model--members node)
                          json-schema-edit-model--members-cache))))
          (cdr (assoc key members)))
      (when-let* ((pair
                   (seq-find
                    (lambda (p)
                      (equal (json-schema-edit-model-pair-key p) key))
                    (json-schema-edit-model-object-pairs node))))
        (json-schema-edit-model-pair-value pair)))))

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

(defun json-schema-edit-model-pointer-at (&optional pos)
  "Return the pointer segments of the value containing POS.
When POS is on an object key, the pointer of that member's value is
returned."
  (let* ((pos (or pos (point)))
         (root (json-schema-edit-model-root))
         (node (treesit-node-at pos 'json))
         segments)
    (while (and node root (not (treesit-node-eq node root)))
      (let ((parent (treesit-node-parent node)))
        (pcase (and parent (treesit-node-type parent))
          ("pair"
           (push (json-schema-edit-model-pair-key parent) segments))
          ("array"
           (when-let* ((idx
                        (seq-position
                         (json-schema-edit-model--value-children parent)
                         node #'treesit-node-eq)))
             (push idx segments))))
        (setq node parent)))
    segments))

(defun json-schema-edit-model-node-at-pointer (segments &optional root)
  "Return the value node reached by SEGMENTS from ROOT.
ROOT defaults to the top-level value of the current buffer."
  (let ((node (or root (json-schema-edit-model-root))))
    (while (and node segments)
      (let ((seg (pop segments)))
        (setq node
              (pcase (treesit-node-type node)
                ("object"
                 (json-schema-edit-model-get node seg))
                ("array"
                 (let ((idx
                        (if (integerp seg)
                            seg
                          (and
                           (stringp seg)
                           (string-match-p "\\`[0-9]+\\'" seg)
                           (string-to-number seg)))))
                   (and idx
                        (nth idx
                             (json-schema-edit-model--value-children node)))))))))
    node))

(defun json-schema-edit-model-normalize-segments (segments)
  "Return SEGMENTS with every segment as a string, for comparison."
  (mapcar #'json-schema-edit-model-key-name segments))

;;;; Schema positions

(defun json-schema-edit-model-schema-pointer-at (&optional pos)
  "Return the pointer of the innermost schema object at POS.
Positions inside keyword values that are not schemas (such as the
string of `type') are mapped to the object that holds them."
  (let ((segments (json-schema-edit-model-pointer-at pos)))
    (while (and segments
                (not (and
                      (json-schema-edit-model-schema-position-p segments)
                      (equal
                       (treesit-node-type
                        (json-schema-edit-model-node-at-pointer segments))
                       "object"))))
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

(defun json-schema-edit-model-schema-position-p (segments)
  "Return non-nil if SEGMENTS address a subschema rather than a keyword value.
The whole path is followed from the root, so that objects inside
`examples', `default' or `enum' are not taken for schemas."
  (let ((state 'schema))
    (dolist (seg segments)
      (setq state
            (pcase state
              ((or 'schema 'schema-or-array)
               (let ((keyword (json-schema-edit-model-keyword seg)))
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
              (_ 'data))))
    (memq state '(schema schema-or-array))))

;;;; Index of $ref / $anchor / $id

(cl-defstruct
    (json-schema-edit-model-ref
     (:constructor json-schema-edit-model-ref-create)
     (:copier nil))
  "A $ref occurrence."
  value     ; the $ref string as written
  pointer   ; segments of the schema object that holds the $ref
  beg end)  ; region of the $ref string literal

(defvar-local json-schema-edit-model--cache nil
  "Cached index of the buffer as (TICK . PLIST).")

(defun json-schema-edit-model--collect (keyword)
  "Return (VALUE-NODE . OBJECT-NODE) for every KEYWORD pair with a string value.
KEYWORD is a symbol."
  (let ((name (symbol-name keyword))
        (captures
         (treesit-query-capture
          (json-schema-edit-model--parser)
          '((pair key: (string) value: (string))
            @pair)))
        result)
    (dolist (c captures)
      (let ((pair (cdr c)))
        (when (equal (json-schema-edit-model-pair-key pair) name)
          (push
           (cons
            (json-schema-edit-model-pair-value pair)
            (treesit-node-parent pair))
           result))))
    (nreverse result)))

(defun json-schema-edit-model--build-index ()
  "Build the index plist of the current buffer."
  (list
   :refs (mapcar
          (lambda (c)
            (let ((v (car c)))
              (json-schema-edit-model-ref-create
               :value (json-schema-edit-model-string-value v)
               :pointer (json-schema-edit-model-pointer-at (treesit-node-start v))
               :beg (treesit-node-start v)
               :end (treesit-node-end v))))
          (json-schema-edit-model--collect '$ref))
   :anchors (mapcar
             (lambda (c)
               (cons
                (json-schema-edit-model-string-value (car c))
                (treesit-node-start (cdr c))))
             (append
              (json-schema-edit-model--collect '$anchor)
              (json-schema-edit-model--collect '$dynamicAnchor)))
   :ids (mapcar
         (lambda (c)
           (cons
            (json-schema-edit-model-string-value (car c))
            (treesit-node-start (cdr c))))
         (json-schema-edit-model--collect '$id))))

(defun json-schema-edit-model-index ()
  "Return the index plist of the current buffer, rebuilding it when stale."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql (car json-schema-edit-model--cache) tick)
      (setq json-schema-edit-model--cache
            (cons tick (json-schema-edit-model--build-index))))
    (cdr json-schema-edit-model--cache)))

(defun json-schema-edit-model-refs ()
  "Return all `json-schema-edit-model-ref' in the current buffer."
  (map-elt (json-schema-edit-model-index) :refs))

(defun json-schema-edit-model-ref-at (&optional pos)
  "Return the `json-schema-edit-model-ref' whose literal contains POS."
  (let ((pos (or pos (point))))
    (seq-find
     (lambda (r)
       (and
        (<= (json-schema-edit-model-ref-beg r) pos)
        (< pos (json-schema-edit-model-ref-end r))))
     (json-schema-edit-model-refs))))

;;;; Draft

(defun json-schema-edit-model-schema-version ()
  "Return the version of the current schema as a symbol.
The whole `$schema' URI is looked up in
`json-schema-edit-schema-versions'.  Return nil when `$schema' is
missing or not listed there."
  (when-let* ((uri
               (json-schema-edit-model-get-string
                (json-schema-edit-model-root)
                '$schema)))
    (cdr (assoc uri json-schema-edit-schema-versions))))

(defun json-schema-edit-model-schema-p ()
  "Return non-nil if the current buffer looks like a JSON Schema."
  (when-let* ((root (json-schema-edit-model-root)))
    (or
     (json-schema-edit-model-get root '$schema)
     (json-schema-edit-model-get root '$defs)
     (json-schema-edit-model-get root 'definitions)
     (seq-some
      (lambda (keyword)
        (json-schema-edit-model-get root keyword))
      json-schema-edit-model--combinators))))

(defun json-schema-edit-model-defs-keyword ()
  "Return the keyword holding definitions in this schema, as a symbol.
An existing `$defs' or `definitions' wins; otherwise it follows the
schema version: `definitions' up to draft-07, `$defs' after."
  (let ((root (json-schema-edit-model-root)))
    (cond
     ((json-schema-edit-model-get root '$defs)
      '$defs)
     ((json-schema-edit-model-get root 'definitions)
      'definitions)
     ((memq
       (json-schema-edit-model-schema-version)
       '(draft-04 draft-06 draft-07))
      'definitions)
     (t '$defs))))

(defun json-schema-edit-model-ref-siblings-ignored-p ()
  "Return non-nil if keywords next to `$ref' are ignored by this version."
  (memq (json-schema-edit-model-schema-version)
        '(draft-04 draft-06 draft-07)))

;;;; Resolving $ref

(defmacro json-schema-edit-model-with-file (file &rest body)
  "Evaluate BODY in a buffer holding FILE with a json parser.
A live buffer visiting FILE is used when it already has a json parser,
so that unsaved edits are seen; otherwise FILE is read into a temporary
buffer."
  (declare (indent 1) (debug t))
  (let ((f (make-symbol "file"))
        (buf (make-symbol "buf")))
    `(let* ((,f ,file)
            (,buf
             (find-buffer-visiting ,f)))
       (if (and ,buf
                (buffer-local-value 'major-mode ,buf)
                (with-current-buffer ,buf
                  (treesit-parser-list nil 'json)))
           (with-current-buffer ,buf ,@body)
         (with-temp-buffer
           (insert-file-contents ,f)
           (setq buffer-file-name nil
                 default-directory
                 (file-name-directory ,f))
           (treesit-parser-create 'json)
           ,@body)))))

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

(defun json-schema-edit-model--resolve-fragment (fragment)
  "Return the start position of FRAGMENT in the current buffer, or nil."
  (cond
   ((or (null fragment) (string-empty-p fragment))
    (when-let* ((root (json-schema-edit-model-root)))
      (treesit-node-start root)))
   ((string-prefix-p "/" fragment)
    (when-let* ((node
                 (json-schema-edit-model-node-at-pointer
                  (json-schema-edit-model-pointer-decode fragment))))
      (treesit-node-start node)))
   (t
    (cdr
     (assoc
      (url-unhex-string fragment)
      (map-elt (json-schema-edit-model-index) :anchors))))))

(defun json-schema-edit-model--id-match-p (id resource)
  "Return non-nil if the `$id' ID names RESOURCE.
Either they are equal, or one is a path suffix of the other, which
covers relative references against an absolute `$id'."
  (let ((id (string-remove-suffix "#" id)))
    (or
     (equal id resource)
     (string-suffix-p (concat "/" resource) id)
     (string-suffix-p (concat "/" id) resource))))

(defun json-schema-edit-model-resolve (ref)
  "Resolve REF from the current buffer.
Return a plist (:file FILE :pos POS :pointer SEGMENTS) where FILE is
nil for the current buffer, or nil when REF cannot be resolved.
Remote references (http etc.) are not resolved."
  (pcase-let* ((`(,resource . ,fragment)
                (json-schema-edit-model-split-ref ref)))
    (cond
     ((string-empty-p resource)
      (when-let* ((pos (json-schema-edit-model--resolve-fragment fragment)))
        (list :file nil :pos pos
              :pointer (json-schema-edit-model-pointer-at pos))))
     ;; A resource embedded in this buffer by `$id'.
     ((when-let* ((id
                   (seq-find
                    (lambda (e)
                      (json-schema-edit-model--id-match-p (car e) resource))
                    (map-elt (json-schema-edit-model-index) :ids)))
                  (base (json-schema-edit-model-pointer-at (cdr id)))
                  (sub (json-schema-edit-model-node-at-pointer base)))
        (let ((node
               (if (and fragment (string-prefix-p "/" fragment))
                   (json-schema-edit-model-node-at-pointer
                    (json-schema-edit-model-pointer-decode fragment)
                    sub)
                 sub)))
          (when node
            (list :file nil :pos
                  (treesit-node-start node)
                  :pointer (json-schema-edit-model-pointer-at
                            (treesit-node-start node)))))))
     ((json-schema-edit-model-remote-p resource)
      nil)
     (t
      (let ((file (json-schema-edit-model-resource-file resource)))
        (when (file-readable-p file)
          (json-schema-edit-model-with-file file
            (when-let* ((pos
                         (json-schema-edit-model--resolve-fragment fragment)))
              (list :file file :pos pos
                    :pointer (json-schema-edit-model-pointer-at pos))))))))))

(defun json-schema-edit-model-resource-file (resource)
  "Return the absolute file name of the relative RESOURCE."
  (let ((path
         (url-unhex-string (string-remove-prefix "file://" resource))))
    (expand-file-name path
                      (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))))

;;;; Summaries

(defun json-schema-edit-model-root-name ()
  "Return a name for the root schema: its title, or the file's base name."
  (or
   (json-schema-edit-model-get-string
    (json-schema-edit-model-root)
    'title)
   (and buffer-file-name
        (file-name-base
         (string-remove-suffix ".schema"
                               (file-name-base buffer-file-name))))
   "root"))

(defun json-schema-edit-model-declared-types (node)
  "Return the types declared by `type' of the schema NODE, or nil."
  (when-let* ((type (json-schema-edit-model-get node 'type)))
    (pcase (treesit-node-type type)
      ("string" (list (json-schema-edit-model-string-value type)))
      ("array"
       (delq nil
             (mapcar #'json-schema-edit-model-string-value
                     (json-schema-edit-model--value-children type)))))))

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
  (when (equal (treesit-node-type node) "object")
    (let ((type (json-schema-edit-model-get node 'type)))
      (cond
       ((null type)
        (when-let* ((ref (json-schema-edit-model-get-string node '$ref)))
          (concat "→ " ref)))
       ((equal (treesit-node-type type) "string")
        (json-schema-edit-model-string-value type))
       ((equal (treesit-node-type type) "array")
        (mapconcat #'json-schema-edit-model-string-value
                   (json-schema-edit-model--value-children type)
                   " | "))))))

(defun json-schema-edit-model-summary (node)
  "Return a one-line summary (type and description) of the schema NODE."
  (when (equal (treesit-node-type node) "object")
    (let ((type (json-schema-edit-model-type-string node))
          (desc
           (or
            (json-schema-edit-model-get-string node 'title)
            (json-schema-edit-model-get-string node 'description))))
      (string-join (delq nil (list type desc)) " — "))))

(provide 'json-schema-edit-model)
;;; json-schema-edit-model.el ends here
