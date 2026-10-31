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
;; Ranges are edited in a sub menu that sets or removes each bound and,
;; for numbers, toggles whether it is exclusive; the menu shows the
;; range in the notation of the outline, such as "(0..100]".  enum is
;; edited one value per line in a small buffer.

;;; Code:

(require 'transient)
(require 'flymake)
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
              (or
               (let ((pending
                      (assq 'type json-schema-edit-constraint--pending)))
                 (when-let* ((pending pending)
                             (v (cdr pending))
                             (v
                              (ignore-errors
                                (json-parse-string
                                 (concat "[" (cdr pending) "]")
                                 :array-type 'list))))
                   (if (listp (car v)) (car v) v)))
               (json-schema-edit-model-declared-types node)
               (and
                (json-schema-edit-model-get node 'properties)
                '("object")))))))

(defun json-schema-edit-constraint--type-p (&rest types)
  "Return non-nil if the schema being edited has one of TYPES."
  (seq-intersection types (json-schema-edit-constraint--types)))

(defun json-schema-edit-constraint--current (keyword)
  "Return the text of KEYWORD, a symbol, taking pending changes into account."
  (if-let* ((pending (assq keyword json-schema-edit-constraint--pending)))
      (cdr pending)
    (json-schema-edit-constraint--in-buffer
      (when-let* ((v
                   (json-schema-edit-model-get
                    (json-schema-edit-constraint--node)
                    keyword)))
        (json-schema-edit-model-node-text v)))))

(defun json-schema-edit-constraint--string (keyword)
  "Return the decoded string value of KEYWORD, or nil."
  (when-let* ((text (json-schema-edit-constraint--current keyword)))
    (ignore-errors
      (car
       (json-parse-string (concat "[" text "]") :array-type 'list)))))

(defun json-schema-edit-constraint--set (&rest changes)
  "Record CHANGES, alternating KEYWORD (a symbol) and TEXT (nil to delete)."
  (while changes
    (let* ((kw (pop changes))
           (text (pop changes))
           (cell (assq kw json-schema-edit-constraint--pending)))
      (if cell
          (setcdr cell text)
        (push (cons kw text) json-schema-edit-constraint--pending)))))

(defun json-schema-edit-constraint--changed-p (keyword)
  "Return non-nil if KEYWORD has a pending change."
  (assq keyword json-schema-edit-constraint--pending))

;;;; Ranges

(defun json-schema-edit-constraint--number (text)
  "Return the number the JSON TEXT denotes, or nil."
  (when text
    (let ((v (ignore-errors (json-parse-string (concat "[" text "]")))))
      (and (vectorp v)
           (= (length v) 1)
           (numberp (aref v 0))
           (aref v 0)))))

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
    (eq
     (json-schema-edit-model-schema-version
      (json-schema-edit-model-current))
     'draft-04)))

(defun json-schema-edit-constraint--range ()
  "Return the current range as a plist (:min :max :xmin :xmax).
:min and :max are the JSON texts of the bounds, nil when absent; :xmin
and :xmax are non-nil when the bound is exclusive."
  (pcase-let ((`(,_ ,min ,max ,xmin ,xmax)
               (json-schema-edit-constraint--range-keywords)))
    (let* ((num
            (lambda (k)
              (let ((v (and k (json-schema-edit-constraint--current k))))
                (and (json-schema-edit-constraint--number v) v))))
           (bool
            (lambda (k)
              (equal
               (and k (json-schema-edit-constraint--current k))
               "true")))
           (lo (funcall num min))
           (hi (funcall num max))
           (xlo (funcall num xmin))
           (xhi (funcall num xmax)))
      ;; draft-04 marks exclusive bounds with booleans.
      (list :min (or xlo lo)
            :max (or xhi hi)
            :xmin (and (or xlo (and lo (funcall bool xmin))) t)
            :xmax (and (or xhi (and hi (funcall bool xmax))) t)))))

(defun json-schema-edit-constraint--range-text (&optional range)
  "Return RANGE (default: the current one) in the range notation, or nil."
  (let* ((range (or range (json-schema-edit-constraint--range)))
         (lo (plist-get range :min))
         (hi (plist-get range :max))
         (xlo (and lo (plist-get range :xmin)))
         (xhi (and hi (plist-get range :xmax))))
    (when (or lo hi)
      (if (or xlo xhi)
          (concat (if xlo "(" "[") (or lo "") ".." (or hi "") (if xhi ")" "]"))
        (concat (or lo "") ".." (or hi ""))))))

(defun json-schema-edit-constraint--check-range (range kind)
  "Signal `user-error' unless the bounds of RANGE make sense for KIND."
  (let ((lo (json-schema-edit-constraint--number (plist-get range :min)))
        (hi (json-schema-edit-constraint--number (plist-get range :max)))
        (exclusive
         (or (and (plist-get range :min) (plist-get range :xmin))
             (and (plist-get range :max) (plist-get range :xmax)))))
    (dolist (side '(:min :max))
      (when-let* ((text (plist-get range side)))
        (let ((n (json-schema-edit-constraint--number text)))
          (unless n
            (user-error "%s is not a number" text))
          (when (and (memq kind '(count integer)) (not (integerp n)))
            (user-error "%s is not an integer" text))
          (when (and (eq kind 'count) (< n 0))
            (user-error "A count cannot be negative")))))
    (when (and (eq kind 'count) exclusive)
      (user-error "Exclusive bounds only apply to numbers"))
    (when (and lo hi (> lo hi))
      (user-error "The lower bound %s is above the upper bound %s"
                  (plist-get range :min) (plist-get range :max)))
    (when (and lo hi (= lo hi) exclusive)
      (user-error "The range is empty"))))

(defun json-schema-edit-constraint-set-range (range)
  "Set the range of the schema being edited to RANGE.
RANGE is a plist like the one `json-schema-edit-constraint--range'
returns.  Signal `user-error' when its bounds do not make sense for the
type; nothing is recorded then.  Only keywords whose text changes are
recorded."
  (pcase-let ((`(,kind ,min ,max ,xmin ,xmax)
               (json-schema-edit-constraint--range-keywords)))
    (unless kind (user-error "No range applies to this type"))
    (json-schema-edit-constraint--check-range range kind)
    (let* ((lo (plist-get range :min))
           (hi (plist-get range :max))
           (xlo (and lo (plist-get range :xmin)))
           (xhi (and hi (plist-get range :xmax)))
           (changes
            (cond
             ((null xmin) (list min lo max hi))
             ((json-schema-edit-constraint--draft-04-p)
              (list min lo max hi
                    xmin (and xlo "true") xmax (and xhi "true")))
             (t (list min (and (not xlo) lo) max (and (not xhi) hi)
                      xmin (and xlo lo) xmax (and xhi hi))))))
      (while changes
        (let ((kw (pop changes))
              (text (pop changes)))
          (unless (equal text (json-schema-edit-constraint--current kw))
            (json-schema-edit-constraint--set kw text)))))))

(defun json-schema-edit-constraint--update-range (&rest changes)
  "Set the range to the current one with CHANGES, a plist, applied."
  (let ((range (copy-sequence (json-schema-edit-constraint--range))))
    (while changes
      (setq range (plist-put range (pop changes) (pop changes))))
    (json-schema-edit-constraint-set-range range)))

;;;; Values

(defun json-schema-edit-constraint-value-text (input type)
  "Return the JSON text for INPUT as a value of TYPE.
For strings INPUT is taken as is; otherwise it must be a JSON literal of
TYPE (any type when TYPE is nil).  Signal `user-error' when it is not."
  (if (equal type "string")
      (json-schema-edit-edit--literal input)
    (let ((parsed
           (condition-case nil
               (json-parse-string
                (concat "[" input "]")
                :null-object :null :false-object :false)
             (error (user-error "Not a JSON value: %s" input)))))
      (unless (= (length parsed) 1)
        (user-error "Give one JSON value: %s" input))
      (let ((v (aref parsed 0)))
        (unless (json-schema-edit-constraint--value-of-type-p v type)
          (user-error "%s is not of type %s"
                      (string-trim input)
                      type)))
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
   (json-schema-edit-model-root
    (json-schema-edit-model-from-string text))))

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
        (pcase-dolist
            (`(,kw . ,text)
             pending)
          (let ((node (json-schema-edit-edit--node pointer)))
            (if text
                (json-schema-edit-edit-set-member node kw text)
              (json-schema-edit-edit-remove-member node kw))))))
    (setq json-schema-edit-constraint--pending nil)
    (message
     (if pending "Wrote %d change(s)" "No change")
     (length pending))))

;;;; Suffixes

(defun json-schema-edit-constraint--show (keyword &optional text)
  "Return TEXT (default: current text of KEYWORD) for the menu."
  (let ((text
         (or text (json-schema-edit-constraint--current keyword))))
    (propertize
     (if text
         (truncate-string-to-width text 40 nil nil "…")
       "(none)")
     'face
     (cond
      ((json-schema-edit-constraint--changed-p keyword)
       'warning)
      (text 'transient-value)
      (t 'shadow)))))

(defun json-schema-edit-constraint--desc (keyword &optional text label)
  "Return a menu description of KEYWORD, a symbol, showing TEXT.
LABEL names KEYWORD in the menu, the name of KEYWORD by default."
  (format "%-13s %s"
          (or label (symbol-name keyword))
          (json-schema-edit-constraint--show keyword text)))

(defmacro json-schema-edit-constraint--with-errors (&rest body)
  "Evaluate BODY, turning a `user-error' into a message so the menu stays."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (user-error (message "%s" (error-message-string err)))))

;;;;; Range

(defun json-schema-edit-constraint--range-face (text &rest keywords)
  "Return the face showing TEXT of a range made of KEYWORDS."
  (cond
   ((seq-some #'json-schema-edit-constraint--changed-p (delq nil keywords))
    'warning)
   (text 'transient-value)
   (t 'shadow)))

(defun json-schema-edit-constraint--range-desc ()
  "Return the menu description of the range."
  (let ((text (json-schema-edit-constraint--range-text)))
    (format "%-13s %s" "range"
            (propertize
             (or text "(none)")
             'face
             (apply #'json-schema-edit-constraint--range-face text
                    (cdr (json-schema-edit-constraint--range-keywords)))))))

(defun json-schema-edit-constraint--range-header ()
  "Return the header of the range menu."
  (format "%s  %s"
          (json-schema-edit-constraint--header)
          (json-schema-edit-constraint--range-desc)))

(defun json-schema-edit-constraint--exclusive-p ()
  "Return non-nil if bounds of the type being edited can be exclusive."
  (nth 3 (json-schema-edit-constraint--range-keywords)))

(defun json-schema-edit-constraint--bound-desc (side)
  "Return the menu description of the bound SIDE, `:min' or `:max'."
  (pcase-let* ((`(,_ ,min ,max ,xmin ,xmax)
                (json-schema-edit-constraint--range-keywords))
               (range (json-schema-edit-constraint--range))
               (text (plist-get range side))
               (`(,label ,kw ,xkw ,flag)
                (if (eq side :min)
                    (list "lower" min xmin :xmin)
                  (list "upper" max xmax :xmax))))
    (format "%-13s %s %s" label
            (propertize
             (or text "(none)")
             'face (json-schema-edit-constraint--range-face text kw xkw))
            (propertize
             (format "%s"
                     (if (and (plist-get range flag)
                              (not (json-schema-edit-constraint--draft-04-p)))
                         xkw
                       kw))
             'face 'shadow))))

(defun json-schema-edit-constraint--exclusive-desc (side)
  "Return the menu description of the exclusiveness of SIDE."
  (let ((exclusive
         (plist-get (json-schema-edit-constraint--range)
                    (if (eq side :min) :xmin :xmax))))
    (format "%-13s %s" "exclusive"
            (propertize (if exclusive "yes" "no")
                        'face (if exclusive 'transient-value 'shadow)))))

(defun json-schema-edit-constraint--read-bound (side)
  "Read the bound SIDE, `:min' or `:max', and record it."
  (json-schema-edit-constraint--with-errors
    (let ((input
           (string-trim
            (read-string
             (format "%s bound (empty to remove): "
                     (if (eq side :min) "Lower" "Upper"))
             (plist-get (json-schema-edit-constraint--range) side)))))
      (json-schema-edit-constraint--update-range
       side (unless (string-empty-p input) input)))))

(defun json-schema-edit-constraint--toggle-exclusive (side)
  "Toggle whether the bound SIDE, `:min' or `:max', is exclusive."
  (json-schema-edit-constraint--with-errors
    (let ((range (json-schema-edit-constraint--range))
          (flag (if (eq side :min) :xmin :xmax)))
      (unless (plist-get range side)
        (user-error "Set the %s bound first"
                    (if (eq side :min) "lower" "upper")))
      (json-schema-edit-constraint--update-range
       flag (not (plist-get range flag))))))

(transient-define-suffix json-schema-edit-constraint-range-min
  ()
  "Set the lower bound."
  :transient t
  :description (lambda () (json-schema-edit-constraint--bound-desc :min))
  (interactive)
  (json-schema-edit-constraint--read-bound :min))

(transient-define-suffix json-schema-edit-constraint-range-max
  ()
  "Set the upper bound."
  :transient t
  :description (lambda () (json-schema-edit-constraint--bound-desc :max))
  (interactive)
  (json-schema-edit-constraint--read-bound :max))

(transient-define-suffix json-schema-edit-constraint-range-remove-min
  ()
  "Remove the lower bound."
  :transient t
  :description "remove lower bound"
  (interactive)
  (json-schema-edit-constraint--with-errors
    (json-schema-edit-constraint--update-range :min nil)))

(transient-define-suffix json-schema-edit-constraint-range-remove-max
  ()
  "Remove the upper bound."
  :transient t
  :description "remove upper bound"
  (interactive)
  (json-schema-edit-constraint--with-errors
    (json-schema-edit-constraint--update-range :max nil)))

(transient-define-suffix json-schema-edit-constraint-range-exclusive-min
  ()
  "Toggle whether the lower bound is exclusive."
  :transient t
  :description (lambda () (json-schema-edit-constraint--exclusive-desc :min))
  (interactive)
  (json-schema-edit-constraint--toggle-exclusive :min))

(transient-define-suffix json-schema-edit-constraint-range-exclusive-max
  ()
  "Toggle whether the upper bound is exclusive."
  :transient t
  :description (lambda () (json-schema-edit-constraint--exclusive-desc :max))
  (interactive)
  (json-schema-edit-constraint--toggle-exclusive :max))

(transient-define-suffix json-schema-edit-constraint-range-clear
  ()
  "Remove both bounds."
  :transient t
  :description "remove both bounds"
  (interactive)
  (json-schema-edit-constraint--with-errors
    (json-schema-edit-constraint--update-range :min nil :max nil)))

(transient-define-prefix json-schema-edit-constraint-range-menu
  ()
  "Edit the range (length, value, items or properties)."
  [:description json-schema-edit-constraint--range-header
                ["Lower"
                 ("m" json-schema-edit-constraint-range-min)
                 ("M" json-schema-edit-constraint-range-remove-min)
                 ("(" json-schema-edit-constraint-range-exclusive-min
                  :if json-schema-edit-constraint--exclusive-p)
                 ]
                ["Upper"
                 ("x" json-schema-edit-constraint-range-max)
                 ("X" json-schema-edit-constraint-range-remove-max)
                 (")" json-schema-edit-constraint-range-exclusive-max
                  :if json-schema-edit-constraint--exclusive-p)
                 ]]
  [("k" json-schema-edit-constraint-range-clear)
   ("RET" "back" transient-quit-one)
   ])

;;;;; Other keywords

(transient-define-suffix json-schema-edit-constraint-pattern
  ()
  "Set `pattern', edited without the JSON string escapes."
  :transient t
  :description (lambda
                 ()
                 (json-schema-edit-constraint--desc
                  'pattern
                  (json-schema-edit-constraint--string 'pattern)))
  (interactive)
  (let ((input
         (read-string "pattern (ECMA-262, empty to remove): "
                      (json-schema-edit-constraint--string 'pattern))))
    (json-schema-edit-constraint--set
     'pattern
     (unless (string-empty-p input)
       (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-format
  ()
  "Set `format'."
  :transient t
  :description (lambda
                 ()
                 (json-schema-edit-constraint--desc
                  'format
                  (json-schema-edit-constraint--string 'format)))
  (interactive)
  (let ((input
         (completing-read "format (empty to remove): "
                          json-schema-edit-constraint-formats nil nil
                          (json-schema-edit-constraint--string 'format))))
    (json-schema-edit-constraint--set
     'format
     (unless (string-empty-p input)
       (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-multiple-of
  ()
  "Set `multipleOf'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'multipleOf))
  (interactive)
  (json-schema-edit-constraint--with-errors
    (let ((input
           (string-trim
            (read-string "multipleOf (empty to remove): "
                         (json-schema-edit-constraint--current 'multipleOf)))))
      (unless (or
               (string-empty-p input)
               (> (or (json-schema-edit-constraint--number input) 0) 0))
        (user-error "The value of `multipleOf' must be a number above 0"))
      (json-schema-edit-constraint--set 'multipleOf
                                        (unless (string-empty-p input)
                                          input)))))

(transient-define-suffix json-schema-edit-constraint-unique
  ()
  "Toggle `uniqueItems'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'uniqueItems))
  (interactive)
  (json-schema-edit-constraint--set
   'uniqueItems
   (unless (equal
            (json-schema-edit-constraint--current 'uniqueItems)
            "true")
     "true")))

(transient-define-suffix json-schema-edit-constraint-items
  ()
  "Set the type of `items'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'items))
  (interactive)
  (let ((type
         (json-schema-edit-constraint--in-buffer
           (json-schema-edit-edit--read-type))))
    (json-schema-edit-constraint--set
     'items
     (unless (string-empty-p type)
       (json-schema-edit-edit--type-schema type)))))

(transient-define-suffix json-schema-edit-constraint-additional
  ()
  "Toggle `additionalProperties' between false and absent."
  :transient t
  :description (lambda
                 ()
                 (json-schema-edit-constraint--desc
                  'additionalProperties nil "additional"))
  (interactive)
  (json-schema-edit-constraint--set
   'additionalProperties
   (unless (equal
            (json-schema-edit-constraint--current 'additionalProperties)
            "false")
     "false")))

(transient-define-suffix json-schema-edit-constraint-property-names
  ()
  "Set the pattern of `propertyNames'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'propertyNames))
  (interactive)
  (let ((input
         (read-string "Pattern of property names (empty to remove): ")))
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
           (initial
            (if (equal type "string")
                (json-schema-edit-constraint--string keyword)
              current))
           (input
            (read-string
             (format "%s%s (empty to remove): " keyword
                     (if type (format " (%s)" type) ""))
             initial)))
      (json-schema-edit-constraint--set
       keyword
       (unless (string-empty-p input)
         (json-schema-edit-constraint-value-text input type))))))

(transient-define-suffix json-schema-edit-constraint-const
  ()
  "Set `const'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'const))
  (interactive)
  (json-schema-edit-constraint--read-value 'const))

(transient-define-suffix json-schema-edit-constraint-default
  ()
  "Set `default'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'default))
  (interactive)
  (json-schema-edit-constraint--read-value 'default))

(transient-define-suffix json-schema-edit-constraint-description
  ()
  "Set `description'."
  :transient t
  :description (lambda () (json-schema-edit-constraint--desc 'description))
  (interactive)
  (let ((input
         (read-string "description (empty to remove): "
                      (json-schema-edit-constraint--string 'description))))
    (json-schema-edit-constraint--set
     'description
     (unless (string-empty-p input)
       (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-examples
  ()
  "Set `examples' as a JSON array."
  :transient t
  :description (lambda
                 ()
                 (let ((n
                        (length
                         (json-schema-edit-constraint--elements
                          (or (json-schema-edit-constraint--current 'examples) "")))))
                   (json-schema-edit-constraint--desc
                    'examples
                    (and
                     (json-schema-edit-constraint--current 'examples)
                     (format "%d item(s)" n)))))
  (interactive)
  (json-schema-edit-constraint--with-errors
    (let ((input
           (string-trim
            (read-string "examples, a JSON array (empty to remove): "
                         (json-schema-edit-constraint--current 'examples)))))
      (unless (or (string-empty-p input) (string-prefix-p "[" input))
        (user-error "The value of `examples' must be an array"))
      (json-schema-edit-constraint--set
       'examples
       (unless (string-empty-p input)
         (json-schema-edit-constraint-value-text input "array"))))))

(transient-define-suffix json-schema-edit-constraint-enum-add
  ()
  "Add one value to `enum'."
  :transient t
  :description "enum: add a value"
  (interactive)
  (json-schema-edit-constraint--with-errors
    (let* ((type (json-schema-edit-constraint--value-type))
           (text
            (json-schema-edit-constraint-value-text
             (read-string
              (format "Add to enum%s: "
                      (if type (format " (%s)" type) "")))
             type))
           (elements (json-schema-edit-constraint--enum-elements)))
      (when (member text elements)
        (user-error "%s is already in enum" text))
      (json-schema-edit-constraint--set
       'enum
       (json-schema-edit-edit--array-text
        (append elements (list text)))))))

(transient-define-suffix json-schema-edit-constraint-enum
  ()
  "Edit `enum' one value per line in a separate buffer."
  :description (lambda () (json-schema-edit-constraint--desc 'enum))
  (interactive)
  (json-schema-edit-enum-edit))

(transient-define-suffix json-schema-edit-constraint-title
  ()
  "Set `title'."
  :transient t
  :description (lambda
                 ()
                 (json-schema-edit-constraint--desc
                  'title
                  (json-schema-edit-constraint--string 'title)))
  (interactive)
  (let ((input
         (read-string "title (empty to remove): "
                      (json-schema-edit-constraint--string 'title))))
    (json-schema-edit-constraint--set
     'title
     (unless (string-empty-p input)
       (json-schema-edit-edit--literal input)))))

(transient-define-suffix json-schema-edit-constraint-view-pending
  ()
  "Show the pending changes in a help window."
  :transient t
  :description (lambda
                 ()
                 (if json-schema-edit-constraint--pending
                     (format "view %d pending change(s)"
                             (length json-schema-edit-constraint--pending))
                   (propertize "no pending change" 'face 'shadow)))
  (interactive)
  (if (null json-schema-edit-constraint--pending)
      (message "No pending change")
    (with-help-window "*JSchema Pending Changes*"
      (with-current-buffer standard-output
        (insert
         (format "Pending changes for %s (written by RET):\n\n"
                 (json-schema-edit-model-pointer-fragment
                  json-schema-edit-constraint--pointer)))
        (dolist (change (reverse json-schema-edit-constraint--pending))
          (insert
           (format "  %-22s %s\n"
                   (car change)
                   (if (cdr change)
                       (truncate-string-to-width
                        (cdr change)
                        60 nil nil "…")
                     "(remove)"))))))))

(transient-define-suffix json-schema-edit-constraint-discard
  ()
  "Discard the pending changes and quit the menu."
  :description "discard changes and quit"
  (interactive)
  (setq json-schema-edit-constraint--pending nil)
  (transient-quit-all))

(transient-define-suffix json-schema-edit-constraint-toggle-all
  ()
  "Toggle listing the keywords that do not apply to the type."
  :transient t
  :description (lambda
                 ()
                 (if json-schema-edit-constraint--show-all
                     "hide inapplicable keywords"
                   "show inapplicable keywords"))
  (interactive)
  (setq json-schema-edit-constraint--show-all
        (not json-schema-edit-constraint--show-all)))

(defun json-schema-edit-constraint--inapplicable ()
  "Return the keywords of the edited schema that do not apply to its type.
They are symbols."
  (let ((types (json-schema-edit-constraint--types)))
    (when types
      (json-schema-edit-constraint--in-buffer
        (seq-filter
         (lambda (k)
           (and
            (not (json-schema-edit-model-keyword-applies-p k types))
            (not (and
                  (assq k json-schema-edit-constraint--pending)
                  (null
                   (cdr (assq k json-schema-edit-constraint--pending)))))))
         (mapcar #'json-schema-edit-model-node-key
                 (json-schema-edit-model-members
                  (json-schema-edit-constraint--node))))))))

(transient-define-suffix json-schema-edit-constraint-remove-inapplicable
  ()
  "Remove the keywords that do not apply to the type."
  :transient t
  :description (lambda
                 ()
                 (let ((ks (json-schema-edit-constraint--inapplicable)))
                   (if ks
                       (concat "remove "
                               (propertize
                                (mapconcat #'symbol-name ks ", ")
                                'face 'warning))
                     (propertize "no inapplicable keyword" 'face 'shadow))))
  (interactive)
  (dolist (k (json-schema-edit-constraint--inapplicable))
    (json-schema-edit-constraint--set k nil)))

(defun json-schema-edit-constraint--header ()
  "Return the header of the constraint menu."
  (let ((name
         (format "%s"
                 (or
                  (car (last json-schema-edit-constraint--pointer))
                  (json-schema-edit-constraint--in-buffer
                    (json-schema-edit-model-root-name
                     (json-schema-edit-model-current)))))))
    (format "%s  %s    %s%s"
            (propertize name 'face 'bold)
            (propertize
             (string-join
              (or (json-schema-edit-constraint--types) '("any"))
              " | ")
             'face 'font-lock-type-face)
            (propertize
             (json-schema-edit-model-pointer-fragment
              json-schema-edit-constraint--pointer)
             'face 'shadow)
            (if json-schema-edit-constraint--pending
                (format "    %s"
                        (propertize
                         (format "%d pending, RET writes"
                                 (length json-schema-edit-constraint--pending))
                         'face 'warning))
              ""))))

(transient-define-prefix json-schema-edit-constraints-menu
  ()
  "Edit the constraints of a schema by its type."
  [:description json-schema-edit-constraint--header
                ["Length and format"
                 :if (lambda
                       ()
                       (json-schema-edit-constraint--type-p "string"))
                 ("l" json-schema-edit-constraint-range-menu
                  :description json-schema-edit-constraint--range-desc)
                 ("p" json-schema-edit-constraint-pattern)
                 ("f" json-schema-edit-constraint-format)
                 ]
                ["Value range"
                 :if (lambda
                       ()
                       (json-schema-edit-constraint--type-p "number" "integer"))
                 ("l" json-schema-edit-constraint-range-menu
                  :description json-schema-edit-constraint--range-desc)
                 ("m" json-schema-edit-constraint-multiple-of)
                 ]
                ["Items"
                 :if (lambda
                       ()
                       (json-schema-edit-constraint--type-p "array"))
                 ("l" json-schema-edit-constraint-range-menu
                  :description json-schema-edit-constraint--range-desc)
                 ("u" json-schema-edit-constraint-unique)
                 ("i" json-schema-edit-constraint-items)
                 ]
                ["Properties"
                 :if (lambda
                       ()
                       (json-schema-edit-constraint--type-p "object"))
                 ("l" json-schema-edit-constraint-range-menu
                  :description json-schema-edit-constraint--range-desc)
                 ("a" json-schema-edit-constraint-additional)
                 ("n" json-schema-edit-constraint-property-names)
                 ]]
  [["Values"
    ("e" json-schema-edit-constraint-enum)
    ("+" json-schema-edit-constraint-enum-add)
    ("c" json-schema-edit-constraint-const)
    ("d" json-schema-edit-constraint-default)
    ]
   ["Annotations"
    ("t" json-schema-edit-constraint-title)
    ("D" json-schema-edit-constraint-description)
    ("x" json-schema-edit-constraint-examples)
    ]]
  [["Inapplicable"
    :if (lambda () json-schema-edit-constraint--show-all)
    ("X" json-schema-edit-constraint-remove-inapplicable)
    ]]
  [("RET" "write changes" json-schema-edit-constraint-apply)
   ("v" json-schema-edit-constraint-view-pending)
   ("Q" json-schema-edit-constraint-discard)
   ("!" json-schema-edit-constraint-toggle-all)
   ])

;;;###autoload
(defun json-schema-edit-constraints (&optional buffer pointer resume)
  "Edit the constraints of the schema at POINTER in BUFFER.
Interactively, the schema at point.  When RESUME is non-nil the pending
changes of the previous menu are kept.  Only one menu session exists
at a time; opening it for another schema discards pending changes,
after saying so."
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (pointer
         (json-schema-edit-model-canonical-pointer
          (or pointer (json-schema-edit-edit-schema-pointer)))))
    (when (and json-schema-edit-constraint--pending
               (not resume)
               (or
                (not (eq json-schema-edit-constraint--buffer buffer))
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
      (unless (json-schema-edit-model-object-p
               (json-schema-edit-constraint--node))
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
        (let ((line
               (buffer-substring-no-properties (pos-bol) (pos-eol))))
          (unless (or (string-prefix-p "#" line) (string-blank-p line))
            (push
             (list
              (pos-bol)
              (pos-eol)
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
    (pcase-dolist
        (`(,beg ,end ,v)
         (json-schema-edit-enum--values))
      (cond
       ((eq (car-safe v) 'error)
        (push (list beg end (cdr v)) problems))
       ((member v seen)
        (push
         (list beg end (format "Duplicate value %s" v))
         problems))
       (t (push v seen))))
    (nreverse problems)))

(defun json-schema-edit-enum-flymake (report-fn &rest _)
  "Flymake backend reporting enum problems to REPORT-FN."
  (funcall report-fn
           (mapcar
            (pcase-lambda
              (`(,beg ,end ,msg))
              (flymake-make-diagnostic
               (current-buffer)
               beg end :error msg))
            (json-schema-edit-enum--problems))))

(defun json-schema-edit-enum-commit ()
  "Record the values as the pending enum and return to the menu."
  (interactive)
  (when-let* ((problem (car (json-schema-edit-enum--problems))))
    (goto-char (nth 0 problem))
    (user-error "%s" (nth 2 problem)))
  (let ((values (mapcar #'caddr (json-schema-edit-enum--values))))
    (json-schema-edit-constraint--set
     'enum
     (and values (json-schema-edit-edit--array-text values)))
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
         (buf
          (get-buffer-create
           (format "*JSchema Enum: %s*"
                   (json-schema-edit-model-pointer-fragment
                    json-schema-edit-constraint--pointer)))))
    (with-current-buffer buf
      (erase-buffer)
      (json-schema-edit-enum-mode)
      (setq json-schema-edit-enum--type type)
      (insert
       (format "# enum: %s (%s)   C-c C-c: done / C-c C-k: discard\n"
               (json-schema-edit-model-pointer-fragment json-schema-edit-constraint--pointer)
               (or type "any")))
      (dolist (e elements)
        (insert
         (if (equal type "string")
             (or (ignore-errors (json-parse-string e)) e)
           e)
         "\n"))
      (goto-char (point-max)))
    (pop-to-buffer buf)))

(provide 'json-schema-edit-constraint)
;;; json-schema-edit-constraint.el ends here
