;;; json-schema-edit-constraint.el --- Editing constraints by type  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui <derutakayu@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; A transient menu listing the keywords that apply to the type of the
;; schema at point, with their current values.  Changes are kept
;; pending and written at once with RET; C-g leaves the buffer as it
;; was.
;;
;; Ranges use the notation of the outline: "1..100", "..100", "1..",
;; and for numbers "(0..100]" with exclusive bounds.  An empty input
;; removes the bounds.  enum is edited one value per line in a small
;; buffer.

;;; Code:

(require 'transient)
(require 'flymake)
(require 'map)
(require 'json-schema-edit-model)
(require 'json-schema-edit-edit)

(defcustom json-schema-edit-constraint-formats
  '("date-time" "date" "time" "duration" "email" "idn-email" "hostname"
    "idn-hostname" "ipv4" "ipv6" "uri" "uri-reference" "iri" "iri-reference"
    "uuid" "uri-template" "json-pointer" "relative-json-pointer" "regex")
  "Values offered for `format'."
  :type '(repeat string)
  :group 'json-schema-edit)

;;;; State

(defvar json-schema-edit-constraint--buffer nil
  "The schema buffer being edited.")

(defvar json-schema-edit-constraint--pointer nil
  "The pointer of the schema being edited.")

(defvar json-schema-edit-constraint--pending nil
  "Pending changes as an alist of (KEYWORD . TEXT), TEXT nil to delete.
KEYWORD is a symbol.")

(defvar json-schema-edit-constraint--show-all nil
  "Non-nil means keywords not applying to the type are listed too.")

(defmacro json-schema-edit-constraint--in-buffer (&rest body)
  "Evaluate BODY in the schema buffer being edited."
  (declare (indent 0) (debug t))
  `(with-current-buffer json-schema-edit-constraint--buffer ,@body))

(defun json-schema-edit-constraint--node ()
  "Return the schema node being edited; call it in the schema buffer."
  (json-schema-edit-edit--node json-schema-edit-constraint--pointer))

(defun json-schema-edit-constraint--types ()
  "Return the declared types of the schema being edited, null excluded."
  (json-schema-edit-constraint--in-buffer
    (let ((node (json-schema-edit-constraint--node)))
      (remove "null"
              (or (let ((pending (assq 'type json-schema-edit-constraint--pending)))
                    (and pending (cdr pending)
                         (json-schema-edit-constraint--parse-types (cdr pending))))
                  (json-schema-edit-model-declared-types node)
                  (and (json-schema-edit-model-get node 'properties) '("object")))))))

(defun json-schema-edit-constraint--parse-types (text)
  "Return the types in the `type' value TEXT."
  (let ((v (ignore-errors (json-parse-string (concat "[" text "]") :array-type 'list))))
    (if (listp (car v)) (car v) v)))

(defun json-schema-edit-constraint--type-p (&rest types)
  "Return non-nil if the schema being edited has one of TYPES."
  (seq-intersection types (json-schema-edit-constraint--types)))

(defun json-schema-edit-constraint--current (keyword)
  "Return the text of KEYWORD, a symbol, taking pending changes into account."
  (if-let* ((pending (assq keyword json-schema-edit-constraint--pending)))
      (cdr pending)
    (json-schema-edit-constraint--in-buffer
      (when-let* ((v (json-schema-edit-model-get (json-schema-edit-constraint--node) keyword)))
        (json-schema-edit-model-node-text v)))))

(defun json-schema-edit-constraint--string (keyword)
  "Return the decoded string value of KEYWORD, or nil."
  (when-let* ((text (json-schema-edit-constraint--current keyword)))
    (ignore-errors (car (json-parse-string (concat "[" text "]") :array-type 'list)))))

(defun json-schema-edit-constraint--set (&rest changes)
  "Record CHANGES, alternating KEYWORD (a symbol) and TEXT (nil to delete)."
  (while changes
    (let* ((kw (pop changes)) (text (pop changes))
           (cell (assq kw json-schema-edit-constraint--pending)))
      (if cell
          (setcdr cell text)
        (push (cons kw text) json-schema-edit-constraint--pending)))))

(defun json-schema-edit-constraint--changed-p (keyword)
  "Return non-nil if KEYWORD has a pending change."
  (assq keyword json-schema-edit-constraint--pending))

;;;; Ranges

(defconst json-schema-edit-constraint--number-re
  "-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][-+]?[0-9]+\\)?"
  "Regexp matching a JSON number.")

(defun json-schema-edit-constraint-parse-range (text kind)
  "Parse the range TEXT for KIND, `count', `integer' or `number'.
Return a plist (:min :max :xmin :xmax) of number strings, nil meaning
absent; signal `user-error' on invalid input.  An empty TEXT gives
all nil."
  (let ((text (string-trim text))
        (num json-schema-edit-constraint--number-re))
    (cond
     ((string-empty-p text) nil)
     ((string-match (concat "\\`\\(" num "\\)\\'") text)
      (json-schema-edit-constraint--check-range
       (list :min (match-string 1 text) :max (match-string 1 text)) kind))
     ((string-match (concat "\\`\\([[(]\\)?[ \t]*\\(" num "\\)?[ \t]*\\.\\.[ \t]*\\("
                            num "\\)?[ \t]*\\([])]\\)?\\'")
                    text)
      (let ((open (match-string 1 text)) (lo (match-string 2 text))
            (hi (match-string 3 text)) (close (match-string 4 text)))
        (when (and (eq kind 'count) (or open close))
          (user-error "Exclusive bounds only apply to numbers"))
        (unless (or lo hi)
          (user-error "Give at least one bound"))
        (json-schema-edit-constraint--check-range
         (list :min (and lo (not (equal open "(")) lo)
               :xmin (and lo (equal open "(") lo)
               :max (and hi (not (equal close ")")) hi)
               :xmax (and hi (equal close ")") hi))
         kind)))
     (t (user-error "Not a range: %s (try 1..100)" text)))))

(defun json-schema-edit-constraint--check-range (range kind)
  "Return RANGE after checking its bounds make sense for KIND."
  (let ((lo (or (map-elt range :min) (map-elt range :xmin)))
        (hi (or (map-elt range :max) (map-elt range :xmax))))
    (dolist (b (list lo hi))
      (when b
        (when (and (memq kind '(count integer)) (not (string-match-p "\\`-?[0-9]+\\'" b)))
          (user-error "%s is not an integer" b))
        (when (and (eq kind 'count) (< (string-to-number b) 0))
          (user-error "A count cannot be negative"))))
    (when (and lo hi (> (string-to-number lo) (string-to-number hi)))
      (user-error "The lower bound %s is above the upper bound %s" lo hi))
    (when (and lo hi (= (string-to-number lo) (string-to-number hi))
               (or (map-elt range :xmin) (map-elt range :xmax)))
      (user-error "The range is empty"))
    range))

(defun json-schema-edit-constraint--range-keywords ()
  "Return (KIND MIN MAX XMIN XMAX) for the type being edited."
  (cond
   ((json-schema-edit-constraint--type-p "string")
    '(count minLength maxLength nil nil))
   ((json-schema-edit-constraint--type-p "integer")
    '(integer minimum maximum exclusiveMinimum exclusiveMaximum))
   ((json-schema-edit-constraint--type-p "number")
    '(number minimum maximum exclusiveMinimum exclusiveMaximum))
   ((json-schema-edit-constraint--type-p "array")
    '(count minItems maxItems nil nil))
   ((json-schema-edit-constraint--type-p "object")
    '(count minProperties maxProperties nil nil))))

(defun json-schema-edit-constraint--draft-04-p ()
  "Return non-nil if the schema being edited is draft-04."
  (json-schema-edit-constraint--in-buffer
    (eq (json-schema-edit-model-schema-version (json-schema-edit-model-current)) 'draft-04)))

(defun json-schema-edit-constraint--range-text ()
  "Return the current range in the range notation, or nil."
  (pcase-let ((`(,_ ,min ,max ,xmin ,xmax) (json-schema-edit-constraint--range-keywords)))
    (let* ((num (lambda (k) (let ((v (and k (json-schema-edit-constraint--current k))))
                              (and v (string-match-p
                                      (concat "\\`" json-schema-edit-constraint--number-re "\\'")
                                      v)
                                   v))))
           (bool (lambda (k) (equal (and k (json-schema-edit-constraint--current k)) "true")))
           (lo (funcall num min)) (hi (funcall num max))
           ;; draft-04 marks exclusive bounds with booleans.
           (xlo (or (funcall num xmin) (and (funcall bool xmin) lo)))
           (xhi (or (funcall num xmax) (and (funcall bool xmax) hi))))
      (when (or lo hi xlo xhi)
        (if (or xlo xhi)
            (concat (if xlo "(" "[") (or xlo lo "") ".." (or xhi hi "") (if xhi ")" "]"))
          (concat (or lo "") ".." (or hi "")))))))

(defun json-schema-edit-constraint-set-range (text)
  "Set the range of the schema being edited from TEXT."
  (pcase-let* ((`(,kind ,min ,max ,xmin ,xmax) (json-schema-edit-constraint--range-keywords))
               (range (json-schema-edit-constraint-parse-range text kind)))
    (unless kind
      (user-error "No range applies to this type"))
    (if (and xmin (json-schema-edit-constraint--draft-04-p))
        (json-schema-edit-constraint--set
         min (or (map-elt range :min) (map-elt range :xmin))
         max (or (map-elt range :max) (map-elt range :xmax))
         xmin (and (map-elt range :xmin) "true")
         xmax (and (map-elt range :xmax) "true"))
      (json-schema-edit-constraint--set min (map-elt range :min) max (map-elt range :max))
      (when xmin
        (json-schema-edit-constraint--set
         xmin (map-elt range :xmin) xmax (map-elt range :xmax))))))

;;;; Values

(defun json-schema-edit-constraint-value-text (input type)
  "Return the JSON text for INPUT as a value of TYPE.
For strings INPUT is taken as is; otherwise it must be a JSON literal of
TYPE (any type when TYPE is nil).  Signal `user-error' when it is not."
  (if (equal type "string")
      (json-schema-edit-edit--literal input)
    (let ((parsed (condition-case nil
                      (json-parse-string (concat "[" input "]")
                                         :null-object :null :false-object :false)
                    (error (user-error "Not a JSON value: %s" input)))))
      (unless (= (length parsed) 1)
        (user-error "Give one JSON value: %s" input))
      (let ((v (aref parsed 0)))
        (unless (json-schema-edit-constraint--value-of-type-p v type)
          (user-error "%s is not of type %s" (string-trim input) type)))
      (string-trim input))))

(defun json-schema-edit-constraint--value-of-type-p (value type)
  "Return non-nil if the parsed VALUE is of TYPE (nil for any)."
  (pcase type
    ('nil t)
    ("string" (stringp value))
    ("integer" (integerp value))
    ("number" (numberp value))
    ("boolean" (memq value '(t :false)))
    ("null" (eq value :null))
    ("array" (vectorp value))
    ("object" (hash-table-p value))
    (_ t)))

(defun json-schema-edit-constraint--value-type ()
  "Return the single type values are checked against, or nil."
  (let ((types (json-schema-edit-constraint--types)))
    (and (= (length types) 1) (car types))))

(defun json-schema-edit-constraint--elements (text)
  "Return the elements of the JSON array TEXT as nodes."
  (json-schema-edit-model-elements
   (json-schema-edit-model-root (json-schema-edit-model-from-string text))))

(defun json-schema-edit-constraint--enum-elements ()
  "Return the texts of the current enum values."
  (when-let* ((text (json-schema-edit-constraint--current 'enum)))
    (mapcar #'json-schema-edit-model-node-text
            (json-schema-edit-constraint--elements text))))

;;;; Applying

(defun json-schema-edit-constraint-apply ()
  "Write the pending changes to the schema buffer as one undoable change."
  (interactive)
  (let ((pending (reverse json-schema-edit-constraint--pending))
        (pointer json-schema-edit-constraint--pointer))
    (json-schema-edit-constraint--in-buffer
      (json-schema-edit-edit--with-undo
        (pcase-dolist (`(,kw . ,text) pending)
          (let ((node (json-schema-edit-edit--node pointer)))
            (if text
                (json-schema-edit-edit-set-member node kw text)
              (json-schema-edit-edit-remove-member node kw))))))
    (setq json-schema-edit-constraint--pending nil)
    (message (if pending "Wrote %d change(s)" "No change") (length pending))))

;;;; Suffixes

(defun json-schema-edit-constraint--show (keyword &optional text)
  "Return TEXT (default: current text of KEYWORD) for the menu."
  (let ((text (or text (json-schema-edit-constraint--current keyword))))
    (propertize (if text (truncate-string-to-width text 40 nil nil "…") "(none)")
                'face (cond ((json-schema-edit-constraint--changed-p keyword) 'warning)
                            (text 'transient-value)
                            (t 'shadow)))))

(defun json-schema-edit-constraint--desc (label keyword &optional text)
  "Return a menu description of LABEL for KEYWORD showing TEXT."
  (format "%-13s %s" label (json-schema-edit-constraint--show keyword text)))

(defmacro json-schema-edit-constraint--with-errors (&rest body)
  "Evaluate BODY, turning a `user-error' into a message so the menu stays."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (user-error (message "%s" (error-message-string err)))))

(transient-define-suffix json-schema-edit-constraint-range ()
  "Set the range (length, value, items or properties)."
  :transient t
  :description (lambda ()
                 (let* ((kws (json-schema-edit-constraint--range-keywords))
                        (changed (seq-some #'json-schema-edit-constraint--changed-p
                                           (delq nil (cdr kws))))
                        (text (json-schema-edit-constraint--range-text)))
                   (format "%-13s %s" "range"
                           (propertize (or text "(none)")
                                       'face (cond (changed 'warning)
                                                   (text 'transient-value)
                                                   (t 'shadow))))))
  (interactive)
  (json-schema-edit-constraint--with-errors
    (json-schema-edit-constraint-set-range
     (read-string "Range (1..100, ..100, (0..1], empty to remove): "
                  (json-schema-edit-constraint--range-text)))))

(transient-define-suffix json-schema-edit-constraint-pattern ()
  "Set `pattern', edited without the JSON string escapes."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc
                           "pattern" 'pattern
                           (json-schema-edit-constraint--string 'pattern)))
  (interactive)
  (let ((input (read-string "pattern (ECMA-262, empty to remove): "
                            (json-schema-edit-constraint--string 'pattern))))
    (json-schema-edit-constraint--set
     'pattern (unless (string-empty-p input) (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-format ()
  "Set `format'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc
                           "format" 'format (json-schema-edit-constraint--string 'format)))
  (interactive)
  (let ((input (completing-read "format (empty to remove): "
                                json-schema-edit-constraint-formats nil nil
                                (json-schema-edit-constraint--string 'format))))
    (json-schema-edit-constraint--set
     'format (unless (string-empty-p input) (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-multiple-of ()
  "Set `multipleOf'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "multipleOf" 'multipleOf))
  (interactive)
  (json-schema-edit-constraint--with-errors
    (let ((input (string-trim (read-string "multipleOf (empty to remove): "
                                           (json-schema-edit-constraint--current 'multipleOf)))))
      (unless (or (string-empty-p input)
                  (and (string-match-p (concat "\\`" json-schema-edit-constraint--number-re "\\'")
                                       input)
                       (> (string-to-number input) 0)))
        (user-error "The value of `multipleOf' must be a number above 0"))
      (json-schema-edit-constraint--set 'multipleOf (unless (string-empty-p input) input)))))

(transient-define-suffix json-schema-edit-constraint-unique ()
  "Toggle `uniqueItems'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "uniqueItems" 'uniqueItems))
  (interactive)
  (json-schema-edit-constraint--set
   'uniqueItems (unless (equal (json-schema-edit-constraint--current 'uniqueItems) "true")
                   "true")))

(transient-define-suffix json-schema-edit-constraint-items ()
  "Set the type of `items'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "items" 'items))
  (interactive)
  (let ((type (json-schema-edit-constraint--in-buffer (json-schema-edit-edit--read-type))))
    (json-schema-edit-constraint--set
     'items (unless (string-empty-p type) (json-schema-edit-edit--type-schema type)))))

(transient-define-suffix json-schema-edit-constraint-additional ()
  "Toggle `additionalProperties' between false and absent."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc
                           "additional" 'additionalProperties))
  (interactive)
  (json-schema-edit-constraint--set
   'additionalProperties
   (unless (equal (json-schema-edit-constraint--current 'additionalProperties) "false")
     "false")))

(transient-define-suffix json-schema-edit-constraint-property-names ()
  "Set the pattern of `propertyNames'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "propertyNames" 'propertyNames))
  (interactive)
  (let ((input (read-string "Pattern of property names (empty to remove): ")))
    (json-schema-edit-constraint--set
     'propertyNames
     (unless (string-empty-p input)
       (json-schema-edit-edit--compact-object-text
        (list (cons 'pattern (json-schema-edit-edit--literal input))))))))

(defun json-schema-edit-constraint--read-value (keyword)
  "Read a value of the edited type for KEYWORD, a symbol, and record it."
  (json-schema-edit-constraint--with-errors
    (let* ((type (json-schema-edit-constraint--value-type))
           (current (json-schema-edit-constraint--current keyword))
           (initial (if (equal type "string")
                        (json-schema-edit-constraint--string keyword)
                      current))
           (input (read-string (format "%s%s (empty to remove): " keyword
                                       (if type (format " (%s)" type) ""))
                               initial)))
      (json-schema-edit-constraint--set
       keyword (unless (string-empty-p input)
                 (json-schema-edit-constraint-value-text input type))))))

(transient-define-suffix json-schema-edit-constraint-const ()
  "Set `const'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "const" 'const))
  (interactive)
  (json-schema-edit-constraint--read-value 'const))

(transient-define-suffix json-schema-edit-constraint-default ()
  "Set `default'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "default" 'default))
  (interactive)
  (json-schema-edit-constraint--read-value 'default))

(transient-define-suffix json-schema-edit-constraint-description ()
  "Set `description'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc "description" 'description))
  (interactive)
  (let ((input (read-string "description (empty to remove): "
                            (json-schema-edit-constraint--string 'description))))
    (json-schema-edit-constraint--set
     'description (unless (string-empty-p input) (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-examples ()
  "Set `examples' as a JSON array."
  :transient t
  :description (lambda ()
                 (let ((n (length (json-schema-edit-constraint--elements
                                   (or (json-schema-edit-constraint--current 'examples) "")))))
                   (json-schema-edit-constraint--desc
                    "examples" 'examples
                    (and (json-schema-edit-constraint--current 'examples)
                         (format "%d item(s)" n)))))
  (interactive)
  (json-schema-edit-constraint--with-errors
    (let ((input (string-trim (read-string "examples, a JSON array (empty to remove): "
                                           (json-schema-edit-constraint--current 'examples)))))
      (unless (or (string-empty-p input) (string-prefix-p "[" input))
        (user-error "The value of `examples' must be an array"))
      (json-schema-edit-constraint--set
       'examples (unless (string-empty-p input)
                    (json-schema-edit-constraint-value-text input "array"))))))

(transient-define-suffix json-schema-edit-constraint-enum-add ()
  "Add one value to `enum'."
  :transient t
  :description "enum: add a value"
  (interactive)
  (json-schema-edit-constraint--with-errors
    (let* ((type (json-schema-edit-constraint--value-type))
           (text (json-schema-edit-constraint-value-text
                  (read-string (format "Add to enum%s: " (if type (format " (%s)" type) "")))
                  type))
           (elements (json-schema-edit-constraint--enum-elements)))
      (when (member text elements)
        (user-error "%s is already in enum" text))
      (json-schema-edit-constraint--set
       'enum (json-schema-edit-edit--array-text (append elements (list text)))))))

(transient-define-suffix json-schema-edit-constraint-enum ()
  "Edit `enum' one value per line in a separate buffer."
  :description (lambda () (json-schema-edit-constraint--desc "enum" 'enum))
  (interactive)
  (json-schema-edit-enum-edit))

(transient-define-suffix json-schema-edit-constraint-title ()
  "Set `title'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc
                           "title" 'title (json-schema-edit-constraint--string 'title)))
  (interactive)
  (let ((input (read-string "title (empty to remove): "
                            (json-schema-edit-constraint--string 'title))))
    (json-schema-edit-constraint--set
     'title (unless (string-empty-p input) (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-view-pending ()
  "Show the pending changes in a help window."
  :transient t
  :description (lambda ()
                 (if json-schema-edit-constraint--pending
                     (format "view %d pending change(s)"
                             (length json-schema-edit-constraint--pending))
                   (propertize "no pending change" 'face 'shadow)))
  (interactive)
  (if (null json-schema-edit-constraint--pending)
      (message "No pending change")
    (with-help-window "*JSchema Pending Changes*"
      (with-current-buffer standard-output
        (insert (format "Pending changes for %s (written by RET):\n\n"
                        (json-schema-edit-model-pointer-fragment
                         json-schema-edit-constraint--pointer)))
        (dolist (change (reverse json-schema-edit-constraint--pending))
          (insert (format "  %-22s %s\n" (car change)
                          (if (cdr change)
                              (truncate-string-to-width (cdr change) 60 nil nil "…")
                            "(remove)"))))))))

(transient-define-suffix json-schema-edit-constraint-discard ()
  "Discard the pending changes and quit the menu."
  :description "discard changes and quit"
  (interactive)
  (setq json-schema-edit-constraint--pending nil)
  (transient-quit-all))

(transient-define-suffix json-schema-edit-constraint-toggle-all ()
  "Toggle listing the keywords that do not apply to the type."
  :transient t
  :description (lambda () (if json-schema-edit-constraint--show-all
                              "hide inapplicable keywords"
                            "show inapplicable keywords"))
  (interactive)
  (setq json-schema-edit-constraint--show-all (not json-schema-edit-constraint--show-all)))

(defun json-schema-edit-constraint--inapplicable ()
  "Return the keywords of the edited schema that do not apply to its type.
They are symbols."
  (let ((types (json-schema-edit-constraint--types)))
    (when types
      (json-schema-edit-constraint--in-buffer
        (seq-filter (lambda (k)
                      (and (not (json-schema-edit-model-keyword-applies-p k types))
                           (not (and (assq k json-schema-edit-constraint--pending)
                                     (null (cdr (assq k json-schema-edit-constraint--pending)))))))
                    (mapcar (lambda (m) (intern (json-schema-edit-model-node-key m)))
                            (json-schema-edit-model-members
                             (json-schema-edit-constraint--node))))))))

(transient-define-suffix json-schema-edit-constraint-remove-inapplicable ()
  "Remove the keywords that do not apply to the type."
  :transient t
  :description (lambda ()
                 (let ((ks (json-schema-edit-constraint--inapplicable)))
                   (if ks
                       (concat "remove " (propertize (mapconcat #'symbol-name ks ", ")
                                                     'face 'warning))
                     (propertize "no inapplicable keyword" 'face 'shadow))))
  (interactive)
  (dolist (k (json-schema-edit-constraint--inapplicable))
    (json-schema-edit-constraint--set k nil)))

(defun json-schema-edit-constraint--header ()
  "Return the header of the constraint menu."
  (let ((name (format "%s" (or (car (last json-schema-edit-constraint--pointer))
                               (json-schema-edit-constraint--in-buffer
                                 (json-schema-edit-model-root-name
                                  (json-schema-edit-model-current)))))))
    (format "%s  %s    %s%s"
            (propertize name 'face 'bold)
            (propertize (string-join (or (json-schema-edit-constraint--types) '("any")) " | ")
                        'face 'font-lock-type-face)
            (propertize (json-schema-edit-model-pointer-fragment
                         json-schema-edit-constraint--pointer)
                        'face 'shadow)
            (if json-schema-edit-constraint--pending
                (format "    %s" (propertize
                                  (format "%d pending, RET writes"
                                          (length json-schema-edit-constraint--pending))
                                  'face 'warning))
              ""))))

(transient-define-prefix json-schema-edit-constraints-menu ()
  "Edit the constraints of a schema by its type."
  [:description json-schema-edit-constraint--header
   ["Length and format"
    :if (lambda () (json-schema-edit-constraint--type-p "string"))
    ("l" json-schema-edit-constraint-range)
    ("p" json-schema-edit-constraint-pattern)
    ("f" json-schema-edit-constraint-format)]
   ["Value range"
    :if (lambda () (json-schema-edit-constraint--type-p "number" "integer"))
    ("l" json-schema-edit-constraint-range)
    ("m" json-schema-edit-constraint-multiple-of)]
   ["Items"
    :if (lambda () (json-schema-edit-constraint--type-p "array"))
    ("l" json-schema-edit-constraint-range)
    ("u" json-schema-edit-constraint-unique)
    ("i" json-schema-edit-constraint-items)]
   ["Properties"
    :if (lambda () (json-schema-edit-constraint--type-p "object"))
    ("l" json-schema-edit-constraint-range)
    ("a" json-schema-edit-constraint-additional)
    ("n" json-schema-edit-constraint-property-names)]]
  [["Values"
    ("e" json-schema-edit-constraint-enum)
    ("+" json-schema-edit-constraint-enum-add)
    ("c" json-schema-edit-constraint-const)
    ("d" json-schema-edit-constraint-default)]
   ["Annotations"
    ("t" json-schema-edit-constraint-title)
    ("D" json-schema-edit-constraint-description)
    ("x" json-schema-edit-constraint-examples)]]
  [["Inapplicable"
    :if (lambda () json-schema-edit-constraint--show-all)
    ("X" json-schema-edit-constraint-remove-inapplicable)]]
  [("RET" "write changes" json-schema-edit-constraint-apply)
   ("v" json-schema-edit-constraint-view-pending)
   ("Q" json-schema-edit-constraint-discard)
   ("!" json-schema-edit-constraint-toggle-all)])

;;;###autoload
(defun json-schema-edit-constraints (&optional buffer pointer resume)
  "Edit the constraints of the schema at POINTER in BUFFER.
Interactively, the schema at point.  When RESUME is non-nil the pending
changes of the previous menu are kept.  Only one menu session exists
at a time; opening it for another schema discards pending changes,
after saying so."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (pointer (or pointer (json-schema-edit-edit-schema-pointer))))
    (when (and json-schema-edit-constraint--pending
               (not resume)
               (or (not (eq json-schema-edit-constraint--buffer buffer))
                   (not (equal json-schema-edit-constraint--pointer pointer))))
      (message "Discarded %d pending change(s) of %s"
               (length json-schema-edit-constraint--pending)
               (json-schema-edit-model-pointer-fragment
                json-schema-edit-constraint--pointer)))
    (setq json-schema-edit-constraint--buffer buffer
          json-schema-edit-constraint--pointer pointer)
    (unless resume
      (setq json-schema-edit-constraint--pending nil
            json-schema-edit-constraint--show-all nil))
    (with-current-buffer buffer
      (unless (json-schema-edit-model-object-p (json-schema-edit-constraint--node))
        (user-error "No schema object at point")))
    (transient-setup 'json-schema-edit-constraints-menu)))

;;;; enum buffer

(defvar json-schema-edit-enum-return-function
  (lambda ()
    (json-schema-edit-constraints json-schema-edit-constraint--buffer
                                  json-schema-edit-constraint--pointer t))
  "Function called after leaving the enum buffer; it reopens the menu.")

(defvar-local json-schema-edit-enum--type nil
  "The type of the values in this enum buffer, or nil.")

(defun json-schema-edit-enum--values ()
  "Return (LINE-BEG LINE-END TEXT-OR-ERROR) for every value line."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (pos-bol) (pos-eol))))
          (unless (or (string-prefix-p "#" line) (string-blank-p line))
            (push (list (pos-bol) (pos-eol)
                        (condition-case err
                            (json-schema-edit-constraint-value-text
                             (if (equal json-schema-edit-enum--type "string")
                                 line
                               (string-trim line))
                             json-schema-edit-enum--type)
                          (user-error (cons 'error (error-message-string err)))))
                  result)))
        (forward-line 1)))
    (nreverse result)))

(defun json-schema-edit-enum--problems ()
  "Return (BEG END MESSAGE) for invalid and duplicated values."
  (let (seen problems)
    (pcase-dolist (`(,beg ,end ,v) (json-schema-edit-enum--values))
      (cond
       ((eq (car-safe v) 'error) (push (list beg end (cdr v)) problems))
       ((member v seen) (push (list beg end (format "Duplicate value %s" v)) problems))
       (t (push v seen))))
    (nreverse problems)))

(defun json-schema-edit-enum-flymake (report-fn &rest _)
  "Flymake backend reporting enum problems to REPORT-FN."
  (funcall report-fn
           (mapcar (pcase-lambda (`(,beg ,end ,msg))
                     (flymake-make-diagnostic (current-buffer) beg end :error msg))
                   (json-schema-edit-enum--problems))))

(defun json-schema-edit-enum-commit ()
  "Record the values as the pending enum and return to the menu."
  (interactive)
  (when-let* ((problem (car (json-schema-edit-enum--problems))))
    (goto-char (nth 0 problem))
    (user-error "%s" (nth 2 problem)))
  (let ((values (mapcar #'caddr (json-schema-edit-enum--values))))
    (json-schema-edit-constraint--set
     'enum (and values (json-schema-edit-edit--array-text values)))
    (json-schema-edit-enum--leave)))

(defun json-schema-edit-enum-abort ()
  "Discard the edits and return to the menu."
  (interactive)
  (json-schema-edit-enum--leave))

(defun json-schema-edit-enum--leave ()
  "Kill the enum buffer and reopen the constraint menu."
  (let ((buf (current-buffer)))
    (quit-window t)
    (when (buffer-live-p buf) (kill-buffer buf)))
  (funcall json-schema-edit-enum-return-function))

(defvar json-schema-edit-enum-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'json-schema-edit-enum-commit)
    (define-key map (kbd "C-c C-k") #'json-schema-edit-enum-abort)
    map)
  "Keymap for `json-schema-edit-enum-mode'.")

(define-derived-mode json-schema-edit-enum-mode text-mode "JSchema-Enum"
  "Major mode for editing enum values, one per line.
Lines starting with # are comments.  For strings, a line is the string
itself; otherwise it is a JSON value.

\\{json-schema-edit-enum-mode-map}"
  (add-hook 'flymake-diagnostic-functions #'json-schema-edit-enum-flymake nil t)
  (flymake-mode 1))

(defun json-schema-edit-enum-edit ()
  "Open a buffer editing the enum of the schema in the constraint menu."
  (let* ((type (json-schema-edit-constraint--value-type))
         (elements (json-schema-edit-constraint--enum-elements))
         (buf (get-buffer-create
               (format "*JSchema Enum: %s*"
                       (json-schema-edit-model-pointer-fragment
                        json-schema-edit-constraint--pointer)))))
    (with-current-buffer buf
      (erase-buffer)
      (json-schema-edit-enum-mode)
      (setq json-schema-edit-enum--type type)
      (insert (format "# enum: %s (%s)   C-c C-c: done / C-c C-k: discard\n"
                      (json-schema-edit-model-pointer-fragment json-schema-edit-constraint--pointer)
                      (or type "any")))
      (dolist (e elements)
        (insert (if (equal type "string")
                    (or (ignore-errors (json-parse-string e)) e)
                  e)
                "\n"))
      (goto-char (point-max)))
    (pop-to-buffer buf)))

(provide 'json-schema-edit-constraint)
;;; json-schema-edit-constraint.el ends here
