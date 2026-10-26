;;; json-schema-edit-outline.el --- Outline of the data shape a schema describes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; A side buffer showing the shape of the instances a schema describes,
;; rather than its syntax tree:
;;
;;   User                       object
;;   ├─ id *                    string (uuid)
;;   ├─ name *                  string  1..100
;;   └─ address                 → Address
;;
;; `*' marks required properties.  Common uses of oneOf / anyOf / allOf
;; are folded into short forms (nullable `string?', unions, discriminated
;; unions, `extends' through allOf); anything else is shown as branches.
;;
;; Folding is `outline-minor-mode' (TAB / S-TAB); headings are found by
;; text property.  Every line carries the JSON Pointer of the schema it
;; shows, which the editing keys act on.  The same tree feeds imenu.

;;; Code:

(require 'outline)
(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-edit)
(require 'json-schema-edit-constraint)

(declare-function json-schema-edit-toggle-nullable "json-schema-edit-transform") (declare-function json-schema-edit-wrap-array "json-schema-edit-transform") (declare-function json-schema-edit-wrap-combinator "json-schema-edit-transform") (declare-function json-schema-edit-extract-definition "json-schema-edit-transform") (declare-function json-schema-edit-inline-ref "json-schema-edit-transform") (declare-function json-schema-edit-combinator-menu "json-schema-edit-menu")

(defcustom json-schema-edit-outline-max-label-width 40
  "Maximum width of the label column; the type column starts after it."
  :type 'integer
  :group 'json-schema-edit)

(defcustom json-schema-edit-outline-max-type-width 36
  "Maximum width of the type column; the note column starts after it."
  :type 'integer
  :group 'json-schema-edit)

(defcustom json-schema-edit-outline-refresh-delay 0.3
  "Idle seconds after a change in the schema before the outline is redrawn."
  :type 'number
  :group 'json-schema-edit)

(defcustom json-schema-edit-outline-display-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4))
  "Display action used to show the outline buffer."
  :type 'sexp
  :group 'json-schema-edit)

(defcustom json-schema-edit-outline-select-window t
  "Non-nil means `json-schema-edit-outline' selects the outline window.
Set it to nil to keep point in the schema buffer."
  :type 'boolean
  :group 'json-schema-edit)

(defface json-schema-edit-outline-heading
  '((t :inherit bold))
  "Face for the root and section lines of the outline."
  :group 'json-schema-edit)

(defface json-schema-edit-outline-definition
  '((t :inherit font-lock-function-name-face))
  "Face for definitions in the outline."
  :group 'json-schema-edit)

(defface json-schema-edit-outline-branch
  '((t :inherit font-lock-keyword-face))
  "Face for combinator branches and `extends' in the outline."
  :group 'json-schema-edit)

(defface json-schema-edit-outline-type
  '((t :inherit font-lock-type-face))
  "Face for the type column of the outline."
  :group 'json-schema-edit)

(defface json-schema-edit-outline-required
  '((t :inherit warning))
  "Face for the required marker of the outline."
  :group 'json-schema-edit)

(defface json-schema-edit-outline-inherited
  '((t :inherit shadow))
  "Face for properties inherited through allOf."
  :group 'json-schema-edit)

;;;; Entries

(cl-defstruct
    (json-schema-edit-outline-entry
     (:constructor json-schema-edit-outline--entry)
     (:copier nil))
  "A line of the outline."
  label     ; shown name
  kind      ; root, section, definition, property, pattern, additional,
  ;; items, branch, const or extends
  pointer   ; segments of the schema the line shows
  pos       ; position in the schema buffer to jump to
  type      ; text of the type column
  note      ; text of the note column
  required  ; non-nil for a required property
  inherited ; name of the allOf base the line comes from
  children)

(defvar json-schema-edit-outline--merge-allof nil
  "Non-nil while building means allOf bases are merged into their users.")

(defvar json-schema-edit-outline--inherited nil
  "While building, the name of the allOf base being expanded.")

(defvar json-schema-edit-outline--visiting nil
  "While building, the pointers of the allOf bases being expanded.")

(defvar json-schema-edit-outline--model nil
  "While building, the model the outline is built from.")

(defun json-schema-edit-outline--node-pos (node)
  "Return the position to jump to for the schema NODE: its member, if any."
  (when node (json-schema-edit-model-member-beg node)))

(defun json-schema-edit-outline--node (pointer)
  "Return the node at POINTER of the model being built from."
  (json-schema-edit-model-node-at-pointer json-schema-edit-outline--model pointer))

(defun json-schema-edit-outline--pos (pointer)
  "Return the position to jump to for the schema at POINTER."
  (json-schema-edit-outline--node-pos
   (json-schema-edit-outline--node pointer)))

(defun json-schema-edit-outline--ref-name (ref)
  "Return a short name for REF: the last segment of its pointer."
  (let ((fragment (cdr (json-schema-edit-model-split-ref ref))))
    (if (and fragment (string-prefix-p "/" fragment))
        (car (last (json-schema-edit-model-pointer-decode fragment)))
      ref)))

(defun json-schema-edit-outline--local-target (node)
  "Return the pointer the `$ref' of NODE points to in this buffer, or nil."
  (when-let* ((ref (json-schema-edit-model-get-string node '$ref))
              (res
               (json-schema-edit-model-resolve json-schema-edit-outline--model ref)))
    (unless (map-elt res :file) (map-elt res :pointer))))

(defun json-schema-edit-outline--text (node &optional width)
  "Return the JSON text of NODE on one line, truncated to WIDTH."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t\n]+" " "
                             (json-schema-edit-model-node-text node))
   (or width 30)
   nil nil "…"))

(defun json-schema-edit-outline--build (node pointer label kind &optional required)
  "Return the entry for the schema NODE at POINTER shown as LABEL of KIND.
REQUIRED marks a required property."
  (pcase-let ((`(,type ,note ,children)
               (json-schema-edit-outline--describe node pointer)))
    (json-schema-edit-outline--entry
     :label label :kind kind :pointer pointer
     :pos (json-schema-edit-outline--node-pos node)
     :type type
     :note (if json-schema-edit-outline--inherited
               (concat "↖ " json-schema-edit-outline--inherited)
             note)
     :required required
     :inherited json-schema-edit-outline--inherited
     :children children)))

(defun json-schema-edit-outline--describe (node pointer)
  "Return (TYPE NOTE CHILDREN) describing the schema NODE at POINTER."
  (pcase (json-schema-edit-model-json-type node)
    ('true
     (list "any" nil nil))
    ('false
     (list "never" nil nil))
    ('object
     (json-schema-edit-outline--describe-object node pointer))
    (_ (list nil nil nil))))

(defun json-schema-edit-outline--describe-object (node pointer)
  "Describe the schema object NODE at POINTER; see `--describe'."
  (let ((comb
         (seq-find
          (lambda (k)
            (json-schema-edit-model-array-p
             (json-schema-edit-model-get node k)))
          '(oneOf anyOf)))
        (own
         (or
          (json-schema-edit-model-get node 'type)
          (json-schema-edit-model-get node 'properties))))
    (cond
     ((and comb (not own))
      (if-let* ((i (json-schema-edit-outline--nullable-branch node comb)))
          (pcase-let* ((ptr (append pointer (list comb i)))
                       (`(,type ,note ,children)
                        (json-schema-edit-outline--describe
                         (json-schema-edit-outline--node ptr)
                         ptr)))
            (list (concat (or type "any") "?") note children))
        (json-schema-edit-outline--describe-combinator node pointer comb)))
     ((json-schema-edit-model-array-p
       (json-schema-edit-model-get node 'allOf))
      (json-schema-edit-outline--describe-all-of node pointer))
     ((and (json-schema-edit-model-get node '$ref) (not own))
      (let ((ref (json-schema-edit-model-get-string node '$ref)))
        (list
         (concat "→ "
                 (and ref (json-schema-edit-outline--ref-name ref)))
         nil nil)))
     (t
      (pcase-let ((`(,type ,note ,children)
                   (json-schema-edit-outline--describe-typed node pointer)))
        (list type note
              (if comb
                  (append children
                          (list
                           (json-schema-edit-outline--combinator-entry
                            node pointer comb)))
                children)))))))

(defun json-schema-edit-outline--null-schema-p (node)
  "Return non-nil if NODE is exactly {\"type\": \"null\"}."
  (let ((members (json-schema-edit-model-members node)))
    (and
     (= (length members) 1)
     (equal (json-schema-edit-model-get-string node 'type) "null"))))

(defun json-schema-edit-outline--nullable-branch (node comb)
  "Return the index of the non-null branch when COMB of NODE is a nullable."
  (let ((branches
         (json-schema-edit-model-elements
          (json-schema-edit-model-get node comb))))
    (when (= (length branches) 2)
      (cond
       ((json-schema-edit-outline--null-schema-p (nth 0 branches))
        1)
       ((json-schema-edit-outline--null-schema-p (nth 1 branches))
        0)))))

;;;; Typed schemas

(defun json-schema-edit-outline--range (node min max &optional xmin xmax)
  "Return the range text of NODE for keywords MIN and MAX, or nil.
XMIN and XMAX are the exclusive variants, written with ( and )."
  (let* ((num
          (lambda (k)
            (when-let* ((v (and k (json-schema-edit-model-get node k))))
              (and
               (eq (json-schema-edit-model-json-type v) 'number)
               (json-schema-edit-model-node-text v)))))
         (lo (funcall num min))
         (hi (funcall num max))
         (xlo (funcall num xmin))
         (xhi (funcall num xmax)))
    (when (or lo hi xlo xhi)
      (if (or xlo xhi)
          (concat
           (if xlo "(" "[")
           (or xlo lo "")
           ".."
           (or xhi hi "")
           (if xhi ")" "]"))
        (concat (or lo "") ".." (or hi ""))))))

(defun json-schema-edit-outline--describe-typed (node pointer)
  "Describe NODE at POINTER by its `type' and constraints."
  (let* ((declared (json-schema-edit-model-declared-types node))
         (types
          (or declared
              (and
               (json-schema-edit-model-get node 'properties)
               '("object"))))
         (nullable (and (member "null" types) (cdr types)))
         (base (if nullable (remove "null" types) types))
         (array-p (equal base '("array")))
         (items (and array-p (json-schema-edit-model-get node 'items)))
         (items-ptr (append pointer '(items)))
         (items-desc
          (and items
               (json-schema-edit-outline--describe items items-ptr)))
         (const (json-schema-edit-model-get node 'const))
         (enum (json-schema-edit-model-get node 'enum))
         (type
          (cond
           (items (concat (or (car items-desc) "any") "[]"))
           (base (string-join base " | "))
           (const (concat "= " (json-schema-edit-outline--text const)))
           (enum "enum")))
         (extras
          (delq nil
                (list
                 (when-let* ((f
                              (json-schema-edit-model-get-string node 'format)))
                   (format "(%s)" f))
                 (cond
                  ((member "string" base)
                   (json-schema-edit-outline--range node 'minLength 'maxLength))
                  ((or (member "number" base) (member "integer" base))
                   (json-schema-edit-outline--range node 'minimum 'maximum
                                                    'exclusiveMinimum 'exclusiveMaximum))
                  ((member "array" base)
                   (json-schema-edit-outline--range node 'minItems 'maxItems))
                  ((member "object" base)
                   (json-schema-edit-outline--range node 'minProperties 'maxProperties)))
                 (json-schema-edit-model-get-string node 'pattern)
                 (when (eq
                        (json-schema-edit-model-json-type
                         (json-schema-edit-model-get node 'uniqueItems))
                        'true)
                   "unique")
                 (when (and enum base)
                   (concat "∈ " (json-schema-edit-outline--text enum)))))))
    (list
     (when type
       (concat type
               (when nullable "?")
               (when extras (concat "  " (string-join extras "  ")))))
     nil
     (append
      (when (or (member "object" base) (null base))
        (json-schema-edit-outline--property-entries node pointer))
      (when items (nth 2 items-desc))
      (when array-p
        (seq-map-indexed
         (lambda (n i)
           (json-schema-edit-outline--build
            n
            (append pointer (list 'prefixItems i))
            (format "[%d]" i)
            'items))
         (json-schema-edit-model-elements
          (json-schema-edit-model-get node 'prefixItems))))))))

(defun json-schema-edit-outline--property-entries (node pointer)
  "Return entries for the properties of the object schema NODE at POINTER."
  (let ((required
         (json-schema-edit-edit--names
          (json-schema-edit-model-get node 'required)))
        (additional
         (json-schema-edit-model-get node 'additionalProperties)))
    (append
     (mapcar
      (lambda (member)
        (let ((name (json-schema-edit-model-node-key member)))
          (json-schema-edit-outline--build
           member
           (append pointer (list 'properties name))
           name 'property
           (member name required))))
      (json-schema-edit-model-members
       (json-schema-edit-model-get node 'properties)))
     (mapcar
      (lambda (member)
        (let ((re (json-schema-edit-model-node-key member)))
          (json-schema-edit-outline--build
           member
           (append pointer (list 'patternProperties re))
           (format "/%s/" re)
           'pattern)))
      (json-schema-edit-model-members
       (json-schema-edit-model-get node 'patternProperties)))
     (when (json-schema-edit-model-object-p additional)
       (list
        (json-schema-edit-outline--build
         additional
         (append pointer '(additionalProperties))
         "{*}" 'additional))))))

;;;; Combinators

(defun json-schema-edit-outline--branches (node comb pointer)
  "Return (POINTER NODE TARGET-POINTER TARGET) for each branch of COMB in NODE.
TARGET is the branch itself, or the local definition its `$ref' points to."
  (seq-map-indexed
   (lambda (branch i)
     (let* ((ptr (append pointer (list comb i)))
            (target-ptr
             (or (json-schema-edit-outline--local-target branch) ptr)))
       (list ptr branch target-ptr
             (json-schema-edit-outline--node target-ptr))))
   (json-schema-edit-model-elements
    (json-schema-edit-model-get node comb))))

(defun json-schema-edit-outline--branch-name (branch index)
  "Return the name of BRANCH at INDEX: title, `$ref' name or number."
  (or
   (json-schema-edit-model-get-string branch 'title)
   (when-let* ((ref (json-schema-edit-model-get-string branch '$ref)))
     (json-schema-edit-outline--ref-name ref))
   (format "#%d" (1+ index))))

(defun json-schema-edit-outline--simple-p (branch)
  "Return non-nil if BRANCH only names a type or a `$ref'."
  (and
   (json-schema-edit-model-object-p branch)
   (seq-every-p
    (lambda (m)
      (memq (json-schema-edit-model-node-key m)
            '(type $ref title description)))
    (json-schema-edit-model-members branch))
   (or
    (json-schema-edit-model-get-string branch 'type)
    (json-schema-edit-model-get-string branch '$ref))))

(defun json-schema-edit-outline--discriminator (node branches)
  "Return the discriminating property of BRANCHES of NODE, or nil.
It is the property every branch fixes with a distinct `const', or the
OpenAPI `discriminator.propertyName'."
  (let ((declared
         (json-schema-edit-model-get-string
          (json-schema-edit-model-get node 'discriminator)
          'propertyName))
        (const-of
         (lambda (target prop)
           (json-schema-edit-model-get
            (json-schema-edit-model-get
             (json-schema-edit-model-get target 'properties)
             prop)
            'const))))
    (or declared
        (when-let* ((first (nth 3 (car branches))))
          (seq-find
           (lambda (prop)
             (let ((values
                    (mapcar
                     (lambda (b)
                       (when-let* ((c (funcall const-of (nth 3 b) prop)))
                         (json-schema-edit-model-node-text c)))
                     branches)))
               (and
                (not (memq nil values))
                (= (length (seq-uniq values)) (length values)))))
           (mapcar #'json-schema-edit-model-node-key
                   (json-schema-edit-model-members
                    (json-schema-edit-model-get first 'properties))))))))

(defun json-schema-edit-outline-branches (model pointer comb)
  "Return the branches of COMB in the schema of MODEL at POINTER.
COMB is a keyword symbol such as `oneOf'.  Each is (POINTER NODE
TARGET-POINTER TARGET), where TARGET is the branch, or the local
definition its `$ref' points to."
  (let ((json-schema-edit-outline--model model))
    (json-schema-edit-outline--branches
     (json-schema-edit-outline--node pointer)
     comb pointer)))

(defun json-schema-edit-outline-discriminator (model pointer comb)
  "Return the property discriminating the COMB branches of MODEL at POINTER."
  (let ((json-schema-edit-outline--model model))
    (json-schema-edit-outline--discriminator
     (json-schema-edit-outline--node pointer)
     (json-schema-edit-outline-branches model pointer comb))))

(defun json-schema-edit-outline--describe-combinator (node pointer comb)
  "Describe NODE at POINTER whose shape is given by the COMB keyword."
  (let* ((branches
          (json-schema-edit-outline--branches node comb pointer))
         (nodes (mapcar #'cadr branches)))
    (cond
     ((seq-every-p
       (lambda (b) (json-schema-edit-model-get b 'const))
       nodes)
      (list
       (truncate-string-to-width
        (mapconcat
         (lambda (b)
           (json-schema-edit-outline--text
            (json-schema-edit-model-get b 'const)))
         nodes " | ")
        json-schema-edit-outline-max-type-width nil nil "…")
       (symbol-name comb)
       (mapcar
        (lambda (b)
          (json-schema-edit-outline--entry
           :label (json-schema-edit-outline--text
                   (json-schema-edit-model-get (cadr b) 'const))
           :kind 'const :pointer
           (car b)
           :pos (json-schema-edit-outline--pos (car b))
           :note (json-schema-edit-model-get-string (cadr b) 'title)
           :inherited json-schema-edit-outline--inherited))
        branches)))
     ((when-let* ((prop
                   (json-schema-edit-outline--discriminator node branches)))
        (list
         (format "%s by %s" comb prop)
         nil
         (mapcar
          (lambda (b)
            (let ((c
                   (json-schema-edit-model-get
                    (json-schema-edit-model-get
                     (json-schema-edit-model-get
                      (nth 3 b)
                      'properties)
                     prop)
                    'const)))
              (json-schema-edit-outline--build
               (nth 3 b)
               (nth 2 b)
               (concat "◇ "
                       (if c
                           (or
                            (json-schema-edit-model-string-value c)
                            (json-schema-edit-model-node-text c))
                         (json-schema-edit-outline--branch-name
                          (cadr b)
                          (seq-position branches b))))
               'branch)))
          branches))))
     ((seq-every-p #'json-schema-edit-outline--simple-p nodes)
      (list
       (mapconcat
        (lambda (b)
          (or
           (json-schema-edit-model-get-string b 'type)
           (json-schema-edit-outline--ref-name
            (json-schema-edit-model-get-string b '$ref))))
        nodes " | ")
       (symbol-name comb)
       nil))
     (t
      (list
       (symbol-name comb)
       nil
       (seq-map-indexed
        (lambda (b i)
          (json-schema-edit-outline--build
           (cadr b)
           (car b)
           (concat "◇ "
                   (json-schema-edit-outline--branch-name (cadr b) i))
           'branch))
        branches))))))

(defun json-schema-edit-outline--combinator-entry (node pointer comb)
  "Return an entry for COMB of NODE at POINTER, shown below its own properties."
  (pcase-let ((`(,type ,note ,children)
               (json-schema-edit-outline--describe-combinator node pointer comb)))
    (json-schema-edit-outline--entry
     :label (symbol-name comb)
     :kind 'branch :pointer pointer
     :pos (json-schema-edit-outline--pos (append pointer (list comb)))
     :type (unless (equal type (symbol-name comb)) type)
     :note note
     :inherited json-schema-edit-outline--inherited
     :children children)))

(defun json-schema-edit-outline--describe-all-of (node pointer)
  "Describe NODE at POINTER that has allOf.
Branches that `$ref' a local definition are shown as `extends', the
others contribute their properties to NODE."
  (let* ((branches
          (json-schema-edit-outline--branches node 'allOf pointer))
         (bases
          (seq-filter
           (lambda (b)
             (not (equal (car b) (nth 2 b))))
           branches)))
    (if (null bases)
        (pcase-let ((`(,type ,note ,children)
                     (json-schema-edit-outline--describe-typed node pointer)))
          (list
           (or type "allOf")
           note
           (append children
                   (list
                    (json-schema-edit-outline--combinator-entry
                     node pointer 'allOf)))))
      (let ((own
             (append
              (json-schema-edit-outline--property-entries node pointer)
              (mapcan
               (lambda (b)
                 (unless (memq b bases)
                   (json-schema-edit-outline--property-entries
                    (cadr b)
                    (car b))))
               branches)))
            extends inherited)
        (dolist (b bases)
          (let* ((name
                  (json-schema-edit-outline--ref-name
                   (json-schema-edit-model-get-string (cadr b) '$ref)))
                 (children
                  (unless (member
                           (nth 2 b)
                           json-schema-edit-outline--visiting)
                    (let ((json-schema-edit-outline--visiting
                           (cons
                            (nth 2 b)
                            json-schema-edit-outline--visiting))
                          (json-schema-edit-outline--inherited name))
                      (nth 2
                           (json-schema-edit-outline--describe
                            (nth 3 b)
                            (nth 2 b)))))))
            (if json-schema-edit-outline--merge-allof
                (setq inherited (append inherited children))
              (push
               (json-schema-edit-outline--entry
                :label (concat "⊂ extends " name)
                :kind 'extends
                :pointer (car b)
                :pos (json-schema-edit-outline--pos (car b))
                :note "allOf" :inherited json-schema-edit-outline--inherited
                :children children)
               extends))))
        (list "object" nil (append inherited own (nreverse extends)))))))

;;;; Tree of the buffer

(defun json-schema-edit-outline-entries (model)
  "Return the top-level outline entries of the schema MODEL.
They are the root schema and one section per definitions keyword."
  (let ((json-schema-edit-outline--model model)
        (root (json-schema-edit-model-root model)))
    (cons
     (json-schema-edit-outline--build root nil
                                      (json-schema-edit-model-root-name model)
                                      'root)
     (delq nil
           (mapcar
            (lambda (kw)
              (when-let* ((defs (json-schema-edit-model-get root kw)))
                (json-schema-edit-outline--entry
                 :label (symbol-name kw)
                 :kind 'section :pointer
                 (list kw)
                 :pos (json-schema-edit-outline--pos (list kw))
                 :children (mapcar
                            (lambda (def)
                              (let ((name (json-schema-edit-model-node-key def)))
                                (json-schema-edit-outline--build
                                 def
                                 (list kw name)
                                 name 'definition)))
                            (json-schema-edit-model-members defs)))))
            '($defs definitions))))))

(defun json-schema-edit-outline-imenu-index ()
  "Return an imenu index built from the outline entries."
  (cl-labels
      ((conv
         (e)
         (let ((label (json-schema-edit-outline-entry-label e))
               (pos (json-schema-edit-outline-entry-pos e))
               (children (json-schema-edit-outline-entry-children e)))
           (if children
               (cons label
                     (cons (cons label pos) (mapcar #'conv children)))
             (cons label pos)))))
    (mapcar #'conv
            (json-schema-edit-outline-entries
             (json-schema-edit-model-current)))))

;;;; Rendering

(defvar-local json-schema-edit-outline--source nil
  "The schema buffer shown by this outline buffer.")

(defvar-local json-schema-edit-outline-merge-allof nil
  "Non-nil means this outline merges allOf bases into their users.")

(defun json-schema-edit-outline--flatten (entries)
  "Return (ENTRY DEPTH PREFIX ID) for ENTRIES and their descendants.
IDs are unique: entries that would share one get a counter suffix."
  (let (result (seen (make-hash-table :test #'equal)))
    (cl-labels
        ((walk
           (es depth prefix parent-id)
           (let ((n (length es))
                 (i 0))
             (dolist (e es)
               (setq i (1+ i))
               (let* ((last (= i n))
                      (base
                       (concat parent-id
                               "/"
                               (json-schema-edit-outline-entry-label e)
                               (when (json-schema-edit-outline-entry-inherited e)
                                 "↖")))
                      (count (1+ (gethash base seen 0)))
                      (id
                       (if (= count 1)
                           base
                         (format "%s#%d" base count)))
                      (glyph
                       (if (zerop depth) "" (if last "└─ " "├─ "))))
                 (puthash base count seen)
                 (push
                  (list e depth (concat prefix glyph) id)
                  result)
                 (walk
                  (json-schema-edit-outline-entry-children e)
                  (1+ depth)
                  (if (zerop depth)
                      ""
                    (concat prefix (if last "   " "│  ")))
                  id))))))
      (walk entries 0 "" ""))
    (nreverse result)))

(defun json-schema-edit-outline--label-face (entry)
  "Return the face of the label of ENTRY."
  (if (json-schema-edit-outline-entry-inherited entry)
      'json-schema-edit-outline-inherited
    (pcase (json-schema-edit-outline-entry-kind entry)
      ((or 'root 'section)
       'json-schema-edit-outline-heading)
      ('definition 'json-schema-edit-outline-definition)
      ((or 'branch 'extends)
       'json-schema-edit-outline-branch))))

(defun json-schema-edit-outline--label (entry prefix)
  "Return the label column text of ENTRY after PREFIX."
  (concat
   (propertize prefix 'face 'shadow)
   (propertize
    (json-schema-edit-outline-entry-label entry)
    'face
    (json-schema-edit-outline--label-face entry)
    'json-schema-edit-outline-name t)
   (when (json-schema-edit-outline-entry-required entry)
     (propertize " *" 'face 'json-schema-edit-outline-required))))

(defun json-schema-edit-outline--render (lines)
  "Insert LINES, as returned by `--flatten', at point."
  (let* ((label-width
          (min json-schema-edit-outline-max-label-width
               (apply #'max 0
                      (mapcar
                       (lambda (l)
                         (string-width
                          (json-schema-edit-outline--label
                           (nth 0 l)
                           (nth 2 l))))
                       lines))))
         (type-col (+ label-width 2))
         (type-width
          (min json-schema-edit-outline-max-type-width
               (apply #'max 0
                      (mapcar
                       (lambda (l)
                         (string-width
                          (or
                           (json-schema-edit-outline-entry-type
                            (nth 0 l))
                           "")))
                       lines))))
         (note-col (+ type-col type-width 2)))
    (pcase-dolist
        (`(,entry ,depth ,prefix ,id)
         lines)
      (let ((beg (point))
            (type (json-schema-edit-outline-entry-type entry))
            (note (json-schema-edit-outline-entry-note entry))
            (inherited
             (json-schema-edit-outline-entry-inherited entry)))
        (insert (json-schema-edit-outline--label entry prefix))
        (when type
          (insert
           (propertize " " 'display `(space :align-to ,type-col))
           (propertize type 'face
                       (if inherited
                           'json-schema-edit-outline-inherited
                         'json-schema-edit-outline-type))))
        (when note
          (insert
           (propertize " " 'display `(space :align-to ,note-col))
           (propertize note 'face 'shadow)))
        (add-text-properties beg
                             (point)
                             (list 'json-schema-edit-outline-entry entry
                                   'json-schema-edit-outline-depth
                                   (1+ depth)
                                   'json-schema-edit-outline-id id))
        (insert "\n")))))

(defun json-schema-edit-outline--line-ids (predicate)
  "Return the ids of the lines at which PREDICATE returns non-nil."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((id
                     (get-text-property
                      (point)
                      'json-schema-edit-outline-id)))
          (when (funcall predicate) (push id ids)))
        (forward-line 1)))
    ids))

(defun json-schema-edit-outline--goto-id (id)
  "Move to the line with ID, or to its nearest existing ancestor.
Return non-nil when a line was found."
  (let ((found nil))
    (while (and id (not found))
      (let ((pos
             (save-excursion
               (goto-char (point-min))
               (text-property-search-forward 'json-schema-edit-outline-id id t))))
        (if pos
            (progn
              (goto-char (prop-match-beginning pos))
              (setq found t))
          (setq id
                (and
                 (string-search "/" id)
                 (substring id 0 (string-match-p "/[^/]*\\'" id)))))
        (when (equal id "") (setq id nil))))
    found))

(defun json-schema-edit-outline-goto-pointer (pointer)
  "Move to the line showing the schema at POINTER, or its nearest ancestor."
  (let ((target (json-schema-edit-model-canonical-pointer pointer))
        best best-len)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((e
                     (get-text-property
                      (point)
                      'json-schema-edit-outline-entry)))
          (let ((p (json-schema-edit-outline-entry-pointer e)))
            (when (and
                   (not (json-schema-edit-outline-entry-inherited e))
                   (equal (seq-take target (length p)) p)
                   (or (null best-len) (> (length p) best-len)))
              (setq best (point) best-len (length p)))))
        (forward-line 1)))
    (when best (goto-char best))))

(defvar-local json-schema-edit-outline--refresh-error nil
  "Message of the last refresh failure that was reported, or nil.")

(defun json-schema-edit-outline-refresh ()
  "Redraw the outline, keeping folded headings and the line at point.
A failure leaves the outline as it was; the error is reported once per
distinct message, so the idle timer cannot flood the echo area."
  (interactive)
  (unless (buffer-live-p json-schema-edit-outline--source)
    (user-error "The schema buffer is gone"))
  (condition-case err
      (progn
        (let* ((folded
                (json-schema-edit-outline--line-ids
                 (lambda () (outline-invisible-p (pos-eol)))))
               (current
                (get-text-property
                 (pos-bol)
                 'json-schema-edit-outline-id))
               (column (current-column))
               (window (get-buffer-window (current-buffer)))
               (start-id
                (when window
                  (get-text-property
                   (window-start window)
                   'json-schema-edit-outline-id)))
               (merge json-schema-edit-outline-merge-allof)
               (lines
                (with-current-buffer json-schema-edit-outline--source
                  (let ((json-schema-edit-outline--merge-allof merge))
                    (json-schema-edit-outline--flatten
                     (json-schema-edit-outline-entries
                      (json-schema-edit-model-current))))))
               (inhibit-read-only t))
          (outline-show-all)
          (erase-buffer)
          (json-schema-edit-outline--render lines)
          (dolist (id folded)
            (save-excursion
              (when (json-schema-edit-outline--goto-id id)
                (outline-hide-subtree))))
          (goto-char (point-min))
          (when (and window start-id
                     (json-schema-edit-outline--goto-id start-id))
            (set-window-start window (pos-bol)))
          (if (and current
                   (json-schema-edit-outline--goto-id current))
              (move-to-column column)
            (goto-char (point-min))))
        (setq json-schema-edit-outline--refresh-error nil))
    (error
     (let ((msg (error-message-string err)))
       (unless (equal msg json-schema-edit-outline--refresh-error)
         (message "Outline refresh failed: %s" msg))
       (setq json-schema-edit-outline--refresh-error msg)))))

;;;; Commands in the outline

(defun json-schema-edit-outline--entry-at-point ()
  "Return the entry of the current line, or signal an error."
  (or
   (get-text-property (pos-bol) 'json-schema-edit-outline-entry)
   (user-error "No schema on this line")))

(defun json-schema-edit-outline--edit (fn)
  "Call FN with the entry at point in the schema buffer, then redraw.
When FN returns a pointer, point moves to its line."
  (let* ((entry (json-schema-edit-outline--entry-at-point))
         (target
          (with-current-buffer json-schema-edit-outline--source
            (save-excursion (funcall fn entry)))))
    (json-schema-edit-outline-refresh)
    (when (and target (listp target))
      (json-schema-edit-outline-goto-pointer target))))

(defun json-schema-edit-outline--property-entry (entry)
  "Return ENTRY if it is an editable property, or signal an error."
  (unless (eq (json-schema-edit-outline-entry-kind entry) 'property)
    (user-error "Not a property"))
  (when (json-schema-edit-outline-entry-inherited entry)
    (message "Editing %s in its base %s"
             (json-schema-edit-outline-entry-label entry)
             (json-schema-edit-outline-entry-inherited entry)))
  entry)

(defun json-schema-edit-outline-visit ()
  "Show the schema at point in the schema buffer and select it."
  (interactive)
  (let ((entry (json-schema-edit-outline--entry-at-point)))
    (pop-to-buffer json-schema-edit-outline--source)
    (goto-char (json-schema-edit-outline-entry-pos entry))))

(defun json-schema-edit-outline-display ()
  "Show the schema at point in the schema buffer without selecting it."
  (interactive)
  (let ((entry (json-schema-edit-outline--entry-at-point))
        (window (display-buffer json-schema-edit-outline--source)))
    (with-selected-window window
      (goto-char (json-schema-edit-outline-entry-pos entry))
      (recenter))))

(defun json-schema-edit-outline--schema-entry (entry)
  "Return ENTRY if it shows an editable schema, or signal an error."
  (unless (memq
           (json-schema-edit-outline-entry-kind entry)
           '(root definition property pattern additional items branch))
    (user-error "No schema on this line"))
  entry)

(defun json-schema-edit-outline-toggle-required ()
  "Toggle whether the property at point is required."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-toggle-required
      (json-schema-edit-outline-entry-pointer
       (json-schema-edit-outline--property-entry e)))
     nil)))

(defun json-schema-edit-outline-change-type ()
  "Change the type of the schema at point."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-change-type
      (json-schema-edit-outline-entry-pointer
       (json-schema-edit-outline--schema-entry e))
      (json-schema-edit-edit--read-type))
     nil)))

(defun json-schema-edit-outline-remove-property ()
  "Remove the property at point, keeping `required' and friends in sync."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-outline--property-entry e)
     (when (y-or-n-p
            (format "Remove property %s? "
                    (json-schema-edit-outline-entry-label e)))
       (json-schema-edit-remove-property
        (json-schema-edit-outline-entry-pointer e)))
     nil)))

(defun json-schema-edit-outline-wrap-array ()
  "Turn the schema at point into an array of it."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-wrap-array
      (json-schema-edit-outline-entry-pointer
       (json-schema-edit-outline--schema-entry e)))
     nil)))

(defun json-schema-edit-outline-wrap-combinator ()
  "Wrap the schema at point in oneOf / anyOf / allOf."
  (interactive)
  (let ((keyword
         (intern
          (completing-read "Wrap in: "
                           (mapcar #'symbol-name
                                   json-schema-edit-model--combinators)
                           nil t nil nil "oneOf"))))
    (json-schema-edit-outline--edit
     (lambda (e)
       (json-schema-edit-wrap-combinator
        (json-schema-edit-outline-entry-pointer
         (json-schema-edit-outline--schema-entry e))
        keyword)
       nil))))

(defun json-schema-edit-outline-extract-definition ()
  "Move the schema at point into the definitions."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (let* ((pointer
             (json-schema-edit-outline-entry-pointer
              (json-schema-edit-outline--schema-entry e)))
            (_
             (when (null pointer)
               (user-error "Cannot extract the root schema")))
            (name
             (read-string "Definition name: "
                          (capitalize
                           (format "%s" (car (last pointer)))))))
       (json-schema-edit-model-pointer-decode
        (cdr
         (json-schema-edit-model-split-ref
          (json-schema-edit-extract-definition pointer name))))))))

(defun json-schema-edit-outline-inline-ref ()
  "Replace the $ref of the schema at point by a copy of its target."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-inline-ref
      (json-schema-edit-outline-entry-pointer
       (json-schema-edit-outline--schema-entry e)))
     nil)))

(defun json-schema-edit-outline-combinator-menu ()
  "Open the branch menu for the oneOf / anyOf / allOf at point."
  (interactive)
  (let ((entry (json-schema-edit-outline--entry-at-point)))
    (pop-to-buffer json-schema-edit-outline--source)
    (goto-char (json-schema-edit-outline-entry-pos entry))
    (call-interactively #'json-schema-edit-combinator-menu)))

(defun json-schema-edit-outline-rename ()
  "Rename the property or definition at point."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (let* ((pointer (json-schema-edit-outline-entry-pointer e))
            (old (car (last pointer))))
       (pcase (json-schema-edit-outline-entry-kind e)
         ('property
          (json-schema-edit-outline--property-entry e)
          (let ((new
                 (read-string
                  (format "Rename property %s to: " old)
                  old)))
            (json-schema-edit-rename-property pointer new)
            (append (butlast pointer) (list new))))
         ('definition
          (let ((new
                 (read-string
                  (format "Rename definition %s to: " old)
                  old)))
            (json-schema-edit-rename-definition pointer new)
            (list (car pointer) new)))
         (_
          (user-error "Only properties and definitions can be renamed")))))))

(defun json-schema-edit-outline-add-property ()
  "Add a property into the object schema at point.
On a property that is not an object schema, add it next to that property."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (let* ((pointer (json-schema-edit-outline-entry-pointer e))
            (owner
             (pcase (json-schema-edit-outline-entry-kind e)
               ('property
                (if (json-schema-edit-edit--object-schema-p
                     (json-schema-edit-edit--node pointer))
                    pointer
                  (butlast pointer 2)))
               ((or 'root 'definition 'branch 'additional 'items)
                pointer)
               (_ (user-error "Cannot add a property here")))))
       (unless (json-schema-edit-edit--object-schema-p
                (json-schema-edit-edit--node owner))
         (user-error "Not an object schema"))
       (let ((name (read-string "Property name: ")))
         (json-schema-edit-add-property owner name
                                        (json-schema-edit-edit--read-type)
                                        (y-or-n-p "Required? "))
         (append owner (list 'properties name)))))))

(defun json-schema-edit-outline-move-property-up ()
  "Move the property at point up among its siblings."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-edit--move-property
      (json-schema-edit-outline-entry-pointer
       (json-schema-edit-outline--property-entry e))
      -1))))

(defun json-schema-edit-outline-move-property-down ()
  "Move the property at point down among its siblings."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-edit--move-property
      (json-schema-edit-outline-entry-pointer
       (json-schema-edit-outline--property-entry e))
      1))))

(defun json-schema-edit-outline-constraints ()
  "Edit the constraints of the schema at point."
  (interactive)
  (let ((entry
         (json-schema-edit-outline--schema-entry
          (json-schema-edit-outline--entry-at-point))))
    (json-schema-edit-constraints json-schema-edit-outline--source
                                  (json-schema-edit-outline-entry-pointer entry))))

(defun json-schema-edit-outline-toggle-nullable ()
  "Toggle whether the schema at point accepts null."
  (interactive)
  (json-schema-edit-outline--edit
   (lambda (e)
     (json-schema-edit-toggle-nullable
      (json-schema-edit-outline-entry-pointer e))
     nil)))

(defun json-schema-edit-outline-toggle-merge ()
  "Toggle between showing allOf bases as `extends' and merging them."
  (interactive)
  (setq json-schema-edit-outline-merge-allof
        (not json-schema-edit-outline-merge-allof))
  (json-schema-edit-outline-refresh)
  (message "allOf: %s"
           (if json-schema-edit-outline-merge-allof "merged" "as written")))

(defun json-schema-edit-outline--goto-name ()
  "Move to the beginning of the name on the current line."
  (when-let* ((pos
               (text-property-any
                (pos-bol) (pos-eol)
                'json-schema-edit-outline-name t)))
    (goto-char pos)))

(defun json-schema-edit-outline-next-line (&optional arg)
  "Move ARG lines down and put point at the beginning of the name."
  (interactive "^p")
  (line-move (or arg 1))
  (json-schema-edit-outline--goto-name))

(defun json-schema-edit-outline-previous-line (&optional arg)
  "Move ARG lines up and put point at the beginning of the name."
  (interactive "^p")
  (line-move (- (or arg 1)))
  (json-schema-edit-outline--goto-name))

(defun json-schema-edit-outline-ignore ()
  "Tell that outline structure editing does not apply here."
  (interactive)
  (user-error "The outline is generated; edit the schema instead"))

(defvar json-schema-edit-outline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'json-schema-edit-outline-visit)
    (define-key map (kbd "C-o") #'json-schema-edit-outline-display)
    (define-key map
                (kbd "*")
                #'json-schema-edit-outline-toggle-required)
    (define-key map (kbd "t") #'json-schema-edit-outline-change-type)
    (define-key map (kbd "r") #'json-schema-edit-outline-rename)
    (define-key map
                (kbd "a")
                #'json-schema-edit-outline-add-property)
    (define-key map
                (kbd "k")
                #'json-schema-edit-outline-remove-property)
    (define-key map
                (kbd "M-p")
                #'json-schema-edit-outline-move-property-up)
    (define-key map
                (kbd "M-n")
                #'json-schema-edit-outline-move-property-down)
    (define-key map (kbd "c") #'json-schema-edit-outline-constraints)
    (define-key map
                (kbd "N")
                #'json-schema-edit-outline-toggle-nullable)
    (define-key map (kbd "[") #'json-schema-edit-outline-wrap-array)
    (define-key map
                (kbd "W")
                #'json-schema-edit-outline-wrap-combinator)
    (define-key map
                (kbd "x")
                #'json-schema-edit-outline-extract-definition)
    (define-key map (kbd "i") #'json-schema-edit-outline-inline-ref)
    (define-key map
                (kbd "o")
                #'json-schema-edit-outline-combinator-menu)
    (define-key map
                (kbd "m")
                #'json-schema-edit-outline-toggle-merge)
    (define-key map (kbd "g") #'json-schema-edit-outline-refresh)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "n") #'json-schema-edit-outline-next-line)
    (define-key map (kbd "p") #'json-schema-edit-outline-previous-line)
    (define-key map
                [remap next-line]
                #'json-schema-edit-outline-next-line)
    (define-key map
                [remap previous-line]
                #'json-schema-edit-outline-previous-line)
    (dolist (cmd
             '(outline-move-subtree-up outline-move-subtree-down
                                       outline-promote outline-demote))
      (define-key map
                  (vector 'remap cmd)
                  #'json-schema-edit-outline-ignore))
    map)
  "Keymap for `json-schema-edit-outline-mode'.")

(define-derived-mode json-schema-edit-outline-mode special-mode "JSchema-Outline"
  "Major mode for the outline of a JSON Schema.

\\{json-schema-edit-outline-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (json-schema-edit-outline-refresh)))
  (setq-local outline-search-function
              (lambda (&optional bound move backward looking-at)
                (outline-search-text-property
                 'json-schema-edit-outline-depth nil bound move backward looking-at)))
  (setq-local outline-level
              (lambda ()
                (or
                 (get-text-property
                  (pos-bol)
                  'json-schema-edit-outline-depth)
                 1)))
  (setq-local outline-minor-mode-cycle t)
  (outline-minor-mode 1))

;;;; Opening and following the schema

(defvar-local json-schema-edit-outline--buffer nil
  "The outline buffer of this schema buffer.")

(defvar-local json-schema-edit-outline--timer nil
  "Idle timer redrawing the outline after a change.")

(defun json-schema-edit-outline--schedule (&rest _)
  "Redraw the outline of this schema buffer when Emacs is idle."
  (when (buffer-live-p json-schema-edit-outline--buffer)
    (when (timerp json-schema-edit-outline--timer)
      (cancel-timer json-schema-edit-outline--timer))
    (let ((outline json-schema-edit-outline--buffer))
      (setq json-schema-edit-outline--timer
            (run-with-idle-timer
             json-schema-edit-outline-refresh-delay nil
             (lambda ()
               (when (buffer-live-p outline)
                 (with-current-buffer outline
                   ;; Refresh reports errors itself, once per message.
                   (json-schema-edit-outline-refresh)))))))))

(defun json-schema-edit-outline-buffer (&optional source)
  "Return the outline buffer of SOURCE (default: current buffer), creating it."
  (let ((source (or source (current-buffer))))
    (with-current-buffer source
      (unless (buffer-live-p json-schema-edit-outline--buffer)
        (setq json-schema-edit-outline--buffer
              (get-buffer-create
               (format "*JSchema Outline: %s*" (buffer-name))))
        (with-current-buffer json-schema-edit-outline--buffer
          (json-schema-edit-outline-mode)
          (setq json-schema-edit-outline--source source)))
      (add-hook 'after-change-functions #'json-schema-edit-outline--schedule nil t)
      json-schema-edit-outline--buffer)))

;;;###autoload
(defun json-schema-edit-outline ()
  "Show the outline of the current schema and move to the schema at point.
The outline window is selected unless
`json-schema-edit-outline-select-window' is nil."
  (interactive)
  (let ((pointer (json-schema-edit-edit-schema-pointer))
        (buf (json-schema-edit-outline-buffer)))
    (with-current-buffer buf
      (json-schema-edit-outline-refresh)
      (json-schema-edit-outline-goto-pointer pointer))
    (let ((window
           (display-buffer buf json-schema-edit-outline-display-action)))
      (when window
        (set-window-point window (with-current-buffer buf (point)))
        (when json-schema-edit-outline-select-window
          (select-window window))))))

(provide 'json-schema-edit-outline)
;;; json-schema-edit-outline.el ends here
