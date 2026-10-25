;;; json-schema-edit-transform.el --- Routine transformations of schemas  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Transformations that are tedious to write by hand:
;;
;; - toggling nullable (a `type' array, or anyOf with {"type": "null"})
;; - turning a schema into an array of it
;; - wrapping a schema in oneOf / anyOf / allOf
;; - extracting a schema into `$defs' and inlining a `$ref'
;; - editing the branches of oneOf / anyOf / allOf: add, remove,
;;   duplicate, move, switch the keyword, set a discriminating property
;;   and factor out properties common to every branch
;;
;; Annotations (title, description, ...) stay on the outer schema when
;; a schema is wrapped, since they describe the place, not the shape.

;;; Code:

(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-edit)
(require 'json-schema-edit-outline)

(defconst json-schema-edit-transform--annotations
  '("title" "description" "$comment" "deprecated" "readOnly" "writeOnly")
  "Keywords kept on the outer schema when a schema is wrapped.")

(defconst json-schema-edit-transform--combinators json-schema-edit-model-combinators
  "Keywords combining subschemas.")

(defun json-schema-edit-transform--node (pointer)
  "Return the schema object at POINTER, or signal an error."
  (let ((node (json-schema-edit-model-node-at-pointer pointer)))
    (unless (equal (treesit-node-type node) "object")
      (user-error "No schema object at point"))
    node))

(defun json-schema-edit-transform--split (node)
  "Split the members of NODE into (ANNOTATIONS . OTHERS)."
  (let (ann rest)
    (dolist (m (json-schema-edit-edit-members node))
      (if (member (car m) json-schema-edit-transform--annotations)
          (push m ann)
        (push m rest)))
    (cons (nreverse ann) (nreverse rest))))

(defun json-schema-edit-transform--wrap (pointer build)
  "Replace the schema at POINTER by a wrapper around it.
BUILD is called with the text of the schema without annotations and
returns the members of the wrapper; annotations are kept in front."
  (pcase-let* ((node (json-schema-edit-transform--node pointer))
               (`(,ann . ,rest) (json-schema-edit-transform--split node))
               (inner (json-schema-edit-edit--compact-object-text rest)))
    (json-schema-edit-edit--with-undo
      (json-schema-edit-edit-replace-node
       node (json-schema-edit-edit--object-text (append ann (funcall build inner)))))))

;;;; Nullable

(defun json-schema-edit-transform--null-branch-p (text)
  "Return non-nil if the member TEXT is {\"type\": \"null\"}."
  (equal (ignore-errors (json-parse-string text :object-type 'alist))
         '((type . "null"))))

(defun json-schema-edit-transform-nullable-p (pointer)
  "Return non-nil if the schema at POINTER accepts null."
  (let* ((node (json-schema-edit-transform--node pointer)))
    (or (member "null" (json-schema-edit-model-declared-types node))
        (seq-some (lambda (comb)
                    (seq-some (lambda (b) (json-schema-edit-transform--null-branch-p
                                           (treesit-node-text b t)))
                              (json-schema-edit-model--value-children
                               (json-schema-edit-model-get node comb))))
                  '("anyOf" "oneOf")))))

(defun json-schema-edit-toggle-nullable (pointer)
  "Toggle whether the schema at POINTER accepts null.
A `type' gets or loses \"null\"; otherwise the schema is wrapped in, or
unwrapped from, anyOf with {\"type\": \"null\"}."
  (interactive (list (json-schema-edit-model-schema-pointer-at)))
  (let* ((node (json-schema-edit-transform--node pointer))
         (type (json-schema-edit-model-get node "type"))
         (types (json-schema-edit-model-declared-types node))
         (comb (seq-find (lambda (c)
                           (seq-some (lambda (b) (json-schema-edit-transform--null-branch-p
                                                  (treesit-node-text b t)))
                                     (json-schema-edit-model--value-children
                                      (json-schema-edit-model-get node c))))
                         '("anyOf" "oneOf"))))
    (cond
     (comb (json-schema-edit-transform--remove-null-branch pointer comb))
     (type
      (let ((new (if (member "null" types) (remove "null" types) (append types '("null")))))
        (json-schema-edit-edit--with-undo
          (json-schema-edit-edit-set-member
           node "type"
           (if (= (length new) 1)
               (json-schema-edit-edit--literal (car new))
             (json-schema-edit-edit--array-text
              (mapcar #'json-schema-edit-edit--literal new)))))))
     (t
      (json-schema-edit-transform--wrap
       pointer (lambda (inner)
                 (list (cons "anyOf" (json-schema-edit-edit--array-text
                                      (list inner "{ \"type\": \"null\" }"))))))))))

(defun json-schema-edit-transform--remove-null-branch (pointer comb)
  "Remove the null branch of COMB at POINTER, unwrapping a single branch."
  (pcase-let* ((node (json-schema-edit-transform--node pointer))
               (branches (json-schema-edit-model--value-children
                          (json-schema-edit-model-get node comb)))
               (others (seq-remove (lambda (b) (json-schema-edit-transform--null-branch-p
                                                (treesit-node-text b t)))
                                   branches))
               (`(,ann . ,rest) (json-schema-edit-transform--split node)))
    (json-schema-edit-edit--with-undo
      (if (and (= (length others) 1) (equal (mapcar #'car rest) (list comb)))
          ;; Only the combinator besides annotations: unwrap.
          (let ((inner (json-schema-edit-edit-members (car others))))
            (json-schema-edit-edit-replace-node
             node (json-schema-edit-edit--object-text
                   (append ann (seq-remove (lambda (m) (assoc (car m) ann)) inner)))))
        (let ((array (json-schema-edit-model-get node comb)))
          (json-schema-edit-edit-remove-element
           array (seq-find (lambda (b) (json-schema-edit-transform--null-branch-p
                                        (treesit-node-text b t)))
                           branches)))))))

;;;; Wrapping

(defun json-schema-edit-wrap-array (pointer)
  "Turn the schema at POINTER into an array of it."
  (interactive (list (json-schema-edit-model-schema-pointer-at)))
  (json-schema-edit-transform--wrap
   pointer (lambda (inner)
             (list (cons "type" "\"array\"") (cons "items" inner)))))

(defun json-schema-edit-wrap-combinator (pointer keyword)
  "Wrap the schema at POINTER in KEYWORD (oneOf, anyOf or allOf)."
  (interactive
   (list (json-schema-edit-model-schema-pointer-at)
         (completing-read "Wrap in: " json-schema-edit-transform--combinators nil t
                          nil nil "oneOf")))
  (json-schema-edit-transform--wrap
   pointer (lambda (inner)
             (list (cons keyword (json-schema-edit-edit--array-text (list inner)))))))

;;;; Definitions

(defun json-schema-edit-extract-definition (pointer name)
  "Move the schema at POINTER into the definitions as NAME.
The schema is replaced by a `$ref' to it."
  (interactive
   (let ((pointer (json-schema-edit-model-schema-pointer-at)))
     (when (null pointer)
       (user-error "Cannot extract the root schema"))
     (list pointer
           (read-string "Definition name: " (capitalize (format "%s" (car (last pointer))))))))
  (let* ((kw (json-schema-edit-model-defs-keyword))
         (node (json-schema-edit-transform--node pointer))
         (text (json-schema-edit-edit-node-text node))
         (ref (json-schema-edit-model-pointer-fragment (list kw name))))
    (when (or (null pointer) (string-empty-p name))
      (user-error "Nothing to extract"))
    (when (json-schema-edit-model-get
           (json-schema-edit-model-get (json-schema-edit-model-root) kw) name)
      (user-error "Definition %s already exists" name))
    (when (equal (seq-take pointer 2) (list kw name))
      (user-error "The schema is that definition"))
    (json-schema-edit-edit--with-undo
      (json-schema-edit-edit-replace-node
       node (json-schema-edit-edit--compact-object-text
             (list (cons "$ref" (json-schema-edit-edit--literal ref)))))
      (let* ((root (json-schema-edit-model-root))
             (defs (json-schema-edit-model-get root kw)))
        (if defs
            (json-schema-edit-edit-add-member defs name text)
          (json-schema-edit-edit-add-member
           root kw (json-schema-edit-edit--object-text (list (cons name text)))))))
    ref))

(defun json-schema-edit-inline-ref (pointer)
  "Replace the local `$ref' of the schema at POINTER by a copy of its target.
Keywords next to the `$ref' are kept and win over the target's.  When
no other `$ref' uses the definition, you are asked whether to delete it."
  (interactive (list (json-schema-edit-model-schema-pointer-at)))
  (let* ((node (json-schema-edit-transform--node pointer))
         (ref (or (json-schema-edit-model-get-string node "$ref")
                  (user-error "No $ref here")))
         (res (json-schema-edit-model-resolve ref))
         (target-ptr (map-elt res :pointer)))
    (unless (and res (not (map-elt res :file)))
      (user-error "Only a $ref into this file can be inlined"))
    (when (equal (seq-take pointer (length target-ptr)) target-ptr)
      (user-error "The $ref points to a schema containing it"))
    (let* ((target (json-schema-edit-model-node-at-pointer target-ptr))
           (own (seq-remove (lambda (m) (equal (car m) "$ref"))
                            (json-schema-edit-edit-members node)))
           (members (append own
                            (seq-remove (lambda (m) (assoc (car m) own))
                                        (if (equal (treesit-node-type target) "object")
                                            (json-schema-edit-edit-members target)
                                          (user-error "The target is not an object schema")))))
           (others (seq-count (lambda (r)
                                (equal (map-elt (json-schema-edit-model-resolve
                                                 (json-schema-edit-model-ref-value r))
                                                :pointer)
                                       target-ptr))
                              (json-schema-edit-model-refs)))
           (delete (and (= others 1)
                        (= (length target-ptr) 2)
                        (member (car target-ptr) '("$defs" "definitions"))
                        (y-or-n-p (format "Delete the definition %s, now unused? "
                                          (cadr target-ptr))))))
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit-replace-node
         node (json-schema-edit-edit--object-text members))
        (when delete
          (let ((defs (json-schema-edit-model-node-at-pointer (list (car target-ptr)))))
            (if (= (length (json-schema-edit-model-object-pairs defs)) 1)
                (json-schema-edit-edit-remove-member (json-schema-edit-model-root)
                                                     (car target-ptr))
              (json-schema-edit-edit-remove-member defs (cadr target-ptr)))))))))

;;;; Combinator branches

(defun json-schema-edit-transform--combinator-at (&optional pointer)
  "Return (OWNER-POINTER KEYWORD INDEX) of the combinator at POINTER.
INDEX is the branch POINTER is in, or nil when POINTER is the owner."
  (let ((pointer (or pointer (json-schema-edit-model-schema-pointer-at)))
        found)
    ;; The innermost branch containing POINTER.
    (let ((p pointer))
      (while (and p (not found))
        (let ((n (length p)))
          (when (and (>= n 2)
                     (member (nth (- n 2) p) json-schema-edit-transform--combinators)
                     (integerp (nth (1- n) p)))
            (setq found (list (butlast p 2) (nth (- n 2) p) (nth (1- n) p)))))
        (setq p (butlast p))))
    (or (let ((node (json-schema-edit-model-node-at-pointer pointer)))
          (when-let* ((kw (seq-find (lambda (k) (json-schema-edit-model-get node k))
                                    json-schema-edit-transform--combinators)))
            (unless (and found (equal (car found) pointer))
              (list pointer kw nil))))
        found
        (user-error "No oneOf / anyOf / allOf at point"))))

(defun json-schema-edit-transform--branch-nodes (owner keyword)
  "Return the branch nodes of KEYWORD in the schema at OWNER."
  (json-schema-edit-model--value-children
   (json-schema-edit-model-get (json-schema-edit-model-node-at-pointer owner) keyword)))

(defun json-schema-edit-add-branch (owner keyword)
  "Add a branch to KEYWORD of the schema at OWNER.
For a discriminated union the discriminating value is asked and the
branch fixes it with `const' and `required'; otherwise a type is asked."
  (interactive (seq-take (json-schema-edit-transform--combinator-at) 2))
  (let* ((node (json-schema-edit-model-node-at-pointer owner))
         (prop (json-schema-edit-outline-discriminator node owner keyword))
         (text
          (if prop
              (let ((value (read-string (format "Value of %s: " prop))))
                (json-schema-edit-edit--object-text
                 (list (cons "type" "\"object\"")
                       (cons "required" (concat "[" (json-schema-edit-edit--literal prop) "]"))
                       (cons "properties"
                             (json-schema-edit-edit--object-text
                              (list (cons prop (json-schema-edit-edit--compact-object-text
                                                (list (cons "const" (json-schema-edit-edit--literal
                                                                     value)))))))))))
            (json-schema-edit-edit--type-schema (json-schema-edit-edit--read-type)))))
    (json-schema-edit-edit--with-undo
      (json-schema-edit-edit-append-element (json-schema-edit-model-get node keyword) text))
    (append owner (list keyword (1- (length (json-schema-edit-transform--branch-nodes
                                             owner keyword)))))))

(defun json-schema-edit-remove-branch (owner keyword index)
  "Remove the branch INDEX of KEYWORD in the schema at OWNER."
  (interactive
   (pcase-let ((`(,owner ,kw ,i) (json-schema-edit-transform--combinator-at)))
     (list owner kw (or i (user-error "Not in a branch")))))
  (let ((branches (json-schema-edit-transform--branch-nodes owner keyword)))
    (json-schema-edit-edit--with-undo
      (json-schema-edit-edit-remove-element
       (json-schema-edit-model-get (json-schema-edit-model-node-at-pointer owner) keyword)
       (or (nth index branches) (user-error "No branch %d" index))))))

(defun json-schema-edit-duplicate-branch (owner keyword index)
  "Insert a copy of the branch INDEX of KEYWORD in the schema at OWNER after it."
  (interactive
   (pcase-let ((`(,owner ,kw ,i) (json-schema-edit-transform--combinator-at)))
     (list owner kw (or i (user-error "Not in a branch")))))
  (let* ((branches (json-schema-edit-transform--branch-nodes owner keyword))
         (branch (or (nth index branches) (user-error "No branch %d" index))))
    (json-schema-edit-edit--with-undo
      (json-schema-edit-edit-insert-after
       branch branches (json-schema-edit-edit-node-text branch)))
    (append owner (list keyword (1+ index)))))

(defun json-schema-edit-move-branch (owner keyword index delta)
  "Move the branch INDEX of KEYWORD in the schema at OWNER by DELTA (-1 or 1)."
  (let* ((branches (json-schema-edit-transform--branch-nodes owner keyword))
         (other (+ index delta)))
    (unless (and (nth index branches) (<= 0 other) (< other (length branches)))
      (user-error "Cannot move the branch further"))
    (let ((a (nth index branches)) (b (nth other branches)))
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit--apply-replacements
         (list (json-schema-edit-edit--node-replacement a (treesit-node-text b t))
               (json-schema-edit-edit--node-replacement b (treesit-node-text a t))))))
    (append owner (list keyword other))))

(defun json-schema-edit-move-branch-up (owner keyword index)
  "Move the branch INDEX of KEYWORD in the schema at OWNER up."
  (interactive
   (pcase-let ((`(,owner ,kw ,i) (json-schema-edit-transform--combinator-at)))
     (list owner kw (or i (user-error "Not in a branch")))))
  (json-schema-edit-move-branch owner keyword index -1))

(defun json-schema-edit-move-branch-down (owner keyword index)
  "Move the branch INDEX of KEYWORD in the schema at OWNER down."
  (interactive
   (pcase-let ((`(,owner ,kw ,i) (json-schema-edit-transform--combinator-at)))
     (list owner kw (or i (user-error "Not in a branch")))))
  (json-schema-edit-move-branch owner keyword index 1))

(defconst json-schema-edit-transform--switch-effects
  '((("oneOf" . "anyOf") . "Looser: a value may now match several branches")
    (("anyOf" . "oneOf") . "Stricter: values matching more than one branch now fail")
    (("allOf" . "oneOf") . "Values must now match exactly one branch, not all")
    (("allOf" . "anyOf") . "Values now need to match only one branch, not all")
    (("oneOf" . "allOf") . "Values must now match every branch")
    (("anyOf" . "allOf") . "Values must now match every branch"))
  "What switching one combinator keyword to another does.")

(defun json-schema-edit-switch-combinator (owner keyword new)
  "Change KEYWORD of the schema at OWNER to NEW, after showing the effect."
  (interactive
   (pcase-let ((`(,owner ,kw ,_) (json-schema-edit-transform--combinator-at)))
     (list owner kw
           (completing-read (format "Switch %s to: " kw)
                            (remove kw json-schema-edit-transform--combinators) nil t))))
  (let* ((node (json-schema-edit-model-node-at-pointer owner))
         (effect (cdr (assoc (cons keyword new) json-schema-edit-transform--switch-effects))))
    (when (json-schema-edit-model-get node new)
      (user-error "The schema already has %s" new))
    (when (or (null effect) (not (called-interactively-p 'any))
              (y-or-n-p (format "%s.  Switch %s to %s? " effect keyword new)))
      (json-schema-edit-edit--with-undo
        (json-schema-edit-edit--apply-replacements
         (list (json-schema-edit-edit--node-replacement
                (treesit-node-child-by-field-name
                 (treesit-node-parent (json-schema-edit-model-get node keyword)) "key")
                (json-schema-edit-edit--literal new)))))
      (when effect (message "%s" effect)))
    (append owner (list new))))

(defun json-schema-edit-set-discriminator (owner keyword prop)
  "Make PROP discriminate the KEYWORD branches of the schema at OWNER.
For every branch the value is asked, and PROP is fixed with `const' and
made required.  A branch that is a local `$ref' is edited in its target."
  (interactive
   (pcase-let ((`(,owner ,kw ,_) (json-schema-edit-transform--combinator-at)))
     (list owner kw (read-string "Discriminating property: "))))
  (when (string-empty-p prop)
    (user-error "Empty property name"))
  (let* ((node (json-schema-edit-model-node-at-pointer owner))
         (branches (json-schema-edit-outline-branches node owner keyword))
         (values
          (seq-map-indexed
           (lambda (b i)
             (let* ((target (nth 3 b))
                    (current (json-schema-edit-model-get
                              (json-schema-edit-model-get
                               (json-schema-edit-model-get target "properties") prop)
                              "const")))
               (read-string (format "Value of %s for branch %s: " prop
                                    (json-schema-edit-outline--branch-name (cadr b) i))
                            (if current
                                (or (json-schema-edit-model-string-value current)
                                    (treesit-node-text current t))
                              (downcase (json-schema-edit-outline--branch-name (cadr b) i))))))
           branches)))
    (unless (= (length (seq-uniq values)) (length values))
      (user-error "Values must differ between branches"))
    (json-schema-edit-edit--with-undo
      (cl-loop for b in branches
               for value in values
               do (let* ((ptr (nth 2 b))
                         (const (json-schema-edit-edit--compact-object-text
                                 (list (cons "const" (json-schema-edit-edit--literal value)))))
                         (props (json-schema-edit-model-node-at-pointer
                                 (append ptr '("properties")))))
                    (cond
                     ((json-schema-edit-model-get props prop)
                      (json-schema-edit-edit-set-member props prop const))
                     (props (json-schema-edit-edit-add-member props prop const))
                     (t (json-schema-edit-edit-add-member
                         (json-schema-edit-model-node-at-pointer ptr) "properties"
                         (json-schema-edit-edit--object-text (list (cons prop const))))))
                    (json-schema-edit-edit--add-required ptr prop))))))

(defun json-schema-edit-factor-common-properties (owner keyword)
  "Move properties identical in every KEYWORD branch to the schema at OWNER.
A property moved is required outside when it was required in every
branch.  Only inline branches are considered."
  (interactive (seq-take (json-schema-edit-transform--combinator-at) 2))
  (let* ((branches (json-schema-edit-transform--branch-nodes owner keyword))
         (props-of (lambda (b) (json-schema-edit-edit-members
                                (json-schema-edit-model-get b "properties"))))
         (common (seq-filter
                  (lambda (m)
                    (seq-every-p (lambda (b) (equal (assoc (car m) (funcall props-of b)) m))
                                 branches))
                  (and branches (funcall props-of (car branches)))))
         (required (seq-filter
                    (lambda (name)
                      (seq-every-p (lambda (b) (member name (json-schema-edit-edit--names
                                                             (json-schema-edit-model-get
                                                              b "required"))))
                                   branches))
                    (mapcar #'car common))))
    (when (or (null common) (< (length branches) 2))
      (user-error "No property is common to every branch"))
    (when (json-schema-edit-model-get
           (json-schema-edit-model-get (json-schema-edit-model-node-at-pointer owner)
                                       "properties")
           (caar common))
      (user-error "Property %s already exists outside" (caar common)))
    (json-schema-edit-edit--with-undo
      ;; Remove from the branches, last first so positions stay valid.
      (dotimes (i (length branches))
        (let ((ptr (append owner (list keyword (- (length branches) i 1)))))
          (dolist (name (mapcar #'car common))
            (let ((branch (json-schema-edit-model-node-at-pointer ptr)))
              (json-schema-edit-edit-remove-member
               (json-schema-edit-model-get branch "properties") name))
            (when (member name required)
              (json-schema-edit-transform--remove-required ptr name)))))
      (dolist (m common)
        (let* ((node (json-schema-edit-model-node-at-pointer owner))
               (props (json-schema-edit-model-get node "properties")))
          (if props
              (json-schema-edit-edit-add-member props (car m) (cdr m))
            (json-schema-edit-edit-add-member
             node "properties" (json-schema-edit-edit--object-text (list m))))))
      (dolist (name required)
        (json-schema-edit-edit--add-required owner name)))
    (mapcar #'car common)))

(defun json-schema-edit-transform--remove-required (pointer name)
  "Remove NAME from `required' of the schema at POINTER, dropping it if empty."
  (let* ((node (json-schema-edit-model-node-at-pointer pointer))
         (required (json-schema-edit-model-get node "required"))
         (elements (json-schema-edit-model--value-children required)))
    (if (equal (json-schema-edit-edit--names required) (list name))
        (json-schema-edit-edit-remove-member node "required")
      (when-let* ((n (seq-find (lambda (n) (equal (json-schema-edit-model-string-value n) name))
                               elements)))
        (json-schema-edit-edit-remove-element required n)))))

(provide 'json-schema-edit-transform)
;;; json-schema-edit-transform.el ends here
