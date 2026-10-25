;;; json-schema-edit-phase4-test.el --- Tests for transforms and constraints  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `json-schema-edit-transform', `json-schema-edit-constraint',
;; the combinator lint and the menus.

;;; Code:

(require 'ert)
(require 'json-schema-edit-mode)

(when-let* ((dir (getenv "TREESIT_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(defmacro json-schema-edit-phase4-test--with-json (text &rest body)
  "Evaluate BODY in a temporary buffer holding the JSON TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (treesit-parser-create 'json)
     (goto-char (point-min))
     ,@body))

(defun json-schema-edit-phase4-test--parse ()
  "Parse the current buffer into alists and lists."
  (save-excursion
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :false-object :false :null-object :null)))

(defmacro json-schema-edit-phase4-test--answers (answers &rest body)
  "Evaluate BODY answering minibuffer questions from ANSWERS in order.
`read-string' and `completing-read' pop strings, `y-or-n-p' pops t or nil."
  (declare (indent 1))
  `(let ((queue (copy-sequence ,answers)))
     (cl-letf
         (((symbol-function 'read-string)
           (lambda (&rest _) (pop queue)))
          ((symbol-function 'completing-read)
           (lambda (&rest _) (pop queue)))
          ((symbol-function 'y-or-n-p)
           (lambda (&rest _) (pop queue))))
       ,@body)))

(defun json-schema-edit-phase4-test--messages ()
  "Return the lint messages of the current buffer."
  (mapcar
   (lambda (d) (nth 3 d))
   (json-schema-edit-lint (json-schema-edit-model-current))))

;;;; Nullable

(ert-deftest json-schema-edit-phase4-test-nullable-type
    ()
  (json-schema-edit-phase4-test--with-json "{\"properties\": {\"a\": {\"type\": \"string\"}}}"
                                           (json-schema-edit-toggle-nullable
                                            '("properties" "a"))
                                           (should
                                            (equal
                                             (json-schema-edit-phase4-test--parse)
                                             '((properties
                                                (a
                                                 (type "string" "null"))))))
                                           (should
                                            (json-schema-edit-transform-nullable-p
                                             '("properties" "a")))
                                           (json-schema-edit-toggle-nullable
                                            '("properties" "a"))
                                           (should
                                            (equal
                                             (buffer-string)
                                             "{\"properties\": {\"a\": {\"type\": \"string\"}}}"))))

(ert-deftest json-schema-edit-phase4-test-nullable-wrap
    ()
  (json-schema-edit-phase4-test--with-json
   "{\n  \"properties\": {\n    \"a\": { \"description\": \"d\", \"$ref\": \"#/$defs/A\" }\n  },\n  \"$defs\": {\"A\": {}}\n}"
   (json-schema-edit-toggle-nullable '("properties" "a"))
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should
      (equal .properties.a
             '((description . "d")
               (anyOf
                (($ref . "#/$defs/A"))
                ((type . "null")))))))
   (should
    (json-schema-edit-transform-nullable-p '("properties" "a")))
   (json-schema-edit-toggle-nullable '("properties" "a"))
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should
      (equal .properties.a
             '((description . "d")
               ($ref . "#/$defs/A")))))))

(ert-deftest json-schema-edit-phase4-test-nullable-keeps-other-branches
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"oneOf\": [{\"type\": \"string\"}, {\"type\": \"null\"}, {\"type\": \"integer\"}]}"
   (json-schema-edit-toggle-nullable nil)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((oneOf
        ((type . "string"))
        ((type . "integer"))))))))

;;;; Wrapping

(ert-deftest json-schema-edit-phase4-test-wrap-array
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"properties\": {\"tags\": {\"title\": \"Tags\", \"type\": \"string\", \"minLength\": 1}}}"
   (json-schema-edit-wrap-array '("properties" "tags"))
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((properties
        (tags
         (title . "Tags")
         (type . "array")
         (items (type . "string") (minLength . 1)))))))))

(ert-deftest json-schema-edit-phase4-test-wrap-combinator
    ()
  (json-schema-edit-phase4-test--with-json "{\"properties\": {\"a\": {\"type\": \"string\"}}}"
                                           (json-schema-edit-wrap-combinator
                                            '("properties" "a")
                                            'oneOf)
                                           (should
                                            (equal
                                             (json-schema-edit-phase4-test--parse)
                                             '((properties
                                                (a
                                                 (oneOf
                                                  ((type . "string"))))))))))

;;;; Definitions

(ert-deftest json-schema-edit-phase4-test-extract-2020-12
    ()
  (json-schema-edit-phase4-test--with-json
   "{\n  \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",\n  \"properties\": {\n    \"address\": {\n      \"type\": \"object\",\n      \"properties\": { \"zip\": { \"type\": \"string\" } }\n    }\n  }\n}"
   (should
    (equal
     (json-schema-edit-extract-definition
      '("properties" "address")
      "Address")
     "#/$defs/Address"))
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should
      (equal .properties.address '(($ref . "#/$defs/Address"))))
     (should
      (equal .$defs.Address
             '((type . "object")
               (properties (zip (type . "string")))))))
   (should (null (json-schema-edit-phase4-test--messages)))
   (should-error
    (json-schema-edit-extract-definition '("properties") "Address")
    :type 'user-error)))

(ert-deftest json-schema-edit-phase4-test-extract-draft-07
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"$schema\": \"https://json-schema.org/draft-07/schema#\", \"items\": {\"type\": \"integer\"}}"
   (json-schema-edit-extract-definition '("items") "Count")
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should (equal .items '(($ref . "#/definitions/Count"))))
     (should (equal .definitions.Count '((type . "integer")))))))

(ert-deftest json-schema-edit-phase4-test-inline-ref
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"properties\": {\"a\": {\"$ref\": \"#/$defs/A\", \"description\": \"own\"}},
        \"$defs\": {\"A\": {\"type\": \"string\", \"description\": \"base\"}}}"
   (json-schema-edit-phase4-test--answers
    '(t)
    (json-schema-edit-inline-ref '("properties" "a")))
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((properties (a (description . "own") (type . "string"))))))))

(ert-deftest json-schema-edit-phase4-test-inline-ref-keeps-used-definition
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"properties\": {\"a\": {\"$ref\": \"#/$defs/A\"}, \"b\": {\"$ref\": \"#/$defs/A\"}},
        \"$defs\": {\"A\": {\"type\": \"string\"}, \"B\": {}}}"
   (json-schema-edit-inline-ref '("properties" "a"))
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should (equal .properties.a '((type . "string"))))
     (should .$defs.A))
   (should-error
    (json-schema-edit-inline-ref '("properties" "a"))
    :type 'user-error)))

;;;; Branches

(defconst json-schema-edit-phase4-test--union
  "{\n  \"oneOf\": [\n    {\n      \"type\": \"object\",\n      \"required\": [\"kind\", \"id\"],\n      \"properties\": {\n        \"kind\": { \"const\": \"a\" },\n        \"id\": { \"type\": \"string\" }\n      }\n    },\n    {\n      \"type\": \"object\",\n      \"required\": [\"kind\", \"id\"],\n      \"properties\": {\n        \"kind\": { \"const\": \"b\" },\n        \"id\": { \"type\": \"string\" }\n      }\n    }\n  ]\n}"
  "A discriminated union used by the branch tests.")

(ert-deftest json-schema-edit-phase4-test-combinator-at
    ()
  (json-schema-edit-phase4-test--with-json json-schema-edit-phase4-test--union
                                           (should
                                            (equal
                                             (json-schema-edit-transform--combinator-at
                                              '())
                                             '(()
                                               oneOf nil)))
                                           (should
                                            (equal
                                             (json-schema-edit-transform--combinator-at
                                              '("oneOf" 1 "properties" "id"))
                                             '(()
                                               oneOf 1)))))

(ert-deftest json-schema-edit-phase4-test-keyword-as-string
    ()
  "Commands take keywords given as strings, as typed by the user."
  (json-schema-edit-phase4-test--with-json json-schema-edit-phase4-test--union
                                           (should
                                            (equal
                                             (json-schema-edit-duplicate-branch nil "oneOf" 0)
                                             '(oneOf 1)))
                                           (should
                                            (equal
                                             (json-schema-edit-switch-combinator nil "oneOf" "anyOf")
                                             '(anyOf)))
                                           (should
                                            (equal
                                             (mapcar #'car
                                                     (json-schema-edit-phase4-test--parse))
                                             '(anyOf))))
  (json-schema-edit-phase4-test--with-json "{\"properties\": {\"a\": {\"type\": \"string\"}}}"
                                           (json-schema-edit-wrap-combinator
                                            '("properties" "a")
                                            "anyOf")
                                           (should
                                            (equal
                                             (json-schema-edit-phase4-test--parse)
                                             '((properties
                                                (a
                                                 (anyOf
                                                  ((type . "string"))))))))))

(ert-deftest json-schema-edit-phase4-test-add-branch-discriminated
    ()
  (json-schema-edit-phase4-test--with-json json-schema-edit-phase4-test--union
                                           (json-schema-edit-phase4-test--answers
                                            '("c")
                                            (should
                                             (equal
                                              (json-schema-edit-add-branch nil 'oneOf)
                                              '(oneOf 2))))
                                           (let-alist
                                               (json-schema-edit-phase4-test--parse)
                                             (should
                                              (equal
                                               (nth 2 .oneOf)
                                               '((type . "object")
                                                 (required "kind")
                                                 (properties
                                                  (kind
                                                   (const . "c")))))))
                                           (should
                                            (null
                                             (json-schema-edit-phase4-test--messages)))))

(ert-deftest json-schema-edit-phase4-test-add-branch-plain
    ()
  (json-schema-edit-phase4-test--with-json "{\"anyOf\": [{\"type\": \"string\"}]}"
                                           (json-schema-edit-phase4-test--answers
                                            '("integer")
                                            (json-schema-edit-add-branch nil 'anyOf))
                                           (should
                                            (equal
                                             (json-schema-edit-phase4-test--parse)
                                             '((anyOf
                                                ((type . "string"))
                                                ((type . "integer"))))))))

(ert-deftest json-schema-edit-phase4-test-remove-duplicate-move
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"anyOf\": [{\"type\": \"string\"}, {\"type\": \"integer\"}, {\"type\": \"null\"}]}"
   (json-schema-edit-remove-branch nil 'anyOf 2)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((anyOf
        ((type . "string"))
        ((type . "integer"))))))
   (should
    (equal
     (json-schema-edit-duplicate-branch nil 'anyOf 0)
     '(anyOf 1)))
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((anyOf
        ((type . "string"))
        ((type . "string"))
        ((type . "integer"))))))
   (json-schema-edit-move-branch-up nil 'anyOf 2)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((anyOf
        ((type . "string"))
        ((type . "integer"))
        ((type . "string"))))))
   (should-error
    (json-schema-edit-move-branch-down nil 'anyOf 2)
    :type 'user-error)))

(ert-deftest json-schema-edit-phase4-test-switch-combinator
    ()
  (json-schema-edit-phase4-test--with-json "{\"oneOf\": [{}], \"anyOf\": []}"
                                           (should-error
                                            (json-schema-edit-switch-combinator nil 'oneOf 'anyOf)
                                            :type 'user-error)
                                           (json-schema-edit-switch-combinator nil 'oneOf 'allOf)
                                           (should
                                            (equal
                                             (json-schema-edit-phase4-test--parse)
                                             '((allOf nil)
                                               (anyOf))))))

(ert-deftest json-schema-edit-phase4-test-set-discriminator
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"oneOf\": [{\"title\": \"Cat\", \"type\": \"object\", \"properties\": {\"lives\": {}}},
                   {\"$ref\": \"#/$defs/Dog\"}],
        \"$defs\": {\"Dog\": {\"type\": \"object\"}}}"
   (json-schema-edit-phase4-test--answers
    '("cat" "dog")
    (json-schema-edit-set-discriminator nil 'oneOf "pet"))
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should
      (equal
       (car .oneOf)
       '((title . "Cat")
         (type . "object")
         (properties (lives) (pet (const . "cat")))
         (required "pet"))))
     (should
      (equal .$defs.Dog
             '((type . "object")
               (properties (pet (const . "dog")))
               (required "pet")))))
   (should
    (equal
     (json-schema-edit-outline-discriminator
      (json-schema-edit-model-current)
      nil 'oneOf)
     "pet"))
   (json-schema-edit-phase4-test--answers
    '("x" "x")
    (should-error
     (json-schema-edit-set-discriminator nil 'oneOf "pet")
     :type 'user-error))))

(ert-deftest json-schema-edit-phase4-test-factor-common
    ()
  (json-schema-edit-phase4-test--with-json json-schema-edit-phase4-test--union
                                           (should
                                            (equal
                                             (json-schema-edit-factor-common-properties nil 'oneOf)
                                             '("id")))
                                           (let-alist
                                               (json-schema-edit-phase4-test--parse)
                                             (should
                                              (equal .properties
                                                     '((id
                                                        (type . "string")))))
                                             (should
                                              (equal .required
                                                     '("id")))
                                             (should
                                              (equal
                                               (car .oneOf)
                                               '((type . "object")
                                                 (required "kind")
                                                 (properties
                                                  (kind
                                                   (const . "a")))))))
                                           (should-error
                                            (json-schema-edit-factor-common-properties nil 'oneOf)
                                            :type 'user-error)))

;;;; Constraint ranges

(ert-deftest json-schema-edit-phase4-test-parse-range
    ()
  (should
   (equal
    (json-schema-edit-constraint-parse-range "1..100" 'count)
    '(:min "1" :xmin nil :max "100" :xmax nil)))
  (should
   (equal
    (json-schema-edit-constraint-parse-range "..100" 'count)
    '(:min nil :xmin nil :max "100" :xmax nil)))
  (should
   (equal
    (json-schema-edit-constraint-parse-range " 1.. " 'count)
    '(:min "1" :xmin nil :max nil :xmax nil)))
  (should
   (equal
    (json-schema-edit-constraint-parse-range "(0..100]" 'number)
    '(:min nil :xmin "0" :max "100" :xmax nil)))
  (should
   (equal
    (json-schema-edit-constraint-parse-range "[-1.5..2e3)" 'number)
    '(:min "-1.5" :xmin nil :max nil :xmax "2e3")))
  (should
   (equal
    (json-schema-edit-constraint-parse-range "5" 'count)
    '(:min "5" :max "5")))
  (should (null (json-schema-edit-constraint-parse-range "" 'count)))
  (dolist (bad
           '(("10..1" count)
             ("-1..3" count)
             ("1.5..3" count)
             ("(1..3]" count)
             ("1.5..3" integer)
             ("(1..1)" number)
             ("abc" number)
             (".." number)))
    (should-error
     (apply #'json-schema-edit-constraint-parse-range bad)
     :type 'user-error)))

(defmacro json-schema-edit-phase4-test--with-constraints (text pointer &rest body)
  "Edit constraints of POINTER in a buffer holding TEXT, then evaluate BODY."
  (declare (indent 2))
  `(json-schema-edit-phase4-test--with-json ,text
                                            (let ((json-schema-edit-constraint--buffer
                                                   (current-buffer))
                                                  (json-schema-edit-constraint--pointer ,pointer)
                                                  (json-schema-edit-constraint--pending nil)
                                                  (json-schema-edit-constraint--show-all nil))
                                              ,@body)))

(ert-deftest json-schema-edit-phase4-test-constraint-string
    ()
  (json-schema-edit-phase4-test--with-constraints
   "{\"type\": \"string\", \"maxLength\": 5, \"minimum\": 1}" nil
   (should (equal (json-schema-edit-constraint--range-text) "..5"))
   (json-schema-edit-constraint-set-range "1..254")
   (should
    (equal (json-schema-edit-constraint--range-text) "1..254"))
   (should
    (equal (json-schema-edit-constraint--inapplicable) '(minimum)))
   (json-schema-edit-phase4-test--answers
    '("^\\d+$" "email")
    (call-interactively #'json-schema-edit-constraint-pattern)
    (call-interactively #'json-schema-edit-constraint-format))
   ;; Nothing is written before applying.
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((type . "string")
       (maxLength . 5)
       (minimum . 1))))
   (call-interactively #'json-schema-edit-constraint-remove-inapplicable)
   (json-schema-edit-constraint-apply)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((type . "string")
       (maxLength . 254)
       (minLength . 1)
       (pattern . "^\\d+$")
       (format . "email"))))
   (should (string-search "\"^\\\\d+$\"" (buffer-string)))
   (should (null json-schema-edit-constraint--pending))))

(ert-deftest json-schema-edit-phase4-test-constraint-number
    ()
  (json-schema-edit-phase4-test--with-constraints
   "{\"type\": \"integer\", \"minimum\": 0}" nil
   (should (equal (json-schema-edit-constraint--range-text) "0.."))
   (json-schema-edit-constraint-set-range "(0..100]")
   (should
    (equal (json-schema-edit-constraint--range-text) "(0..100]"))
   (should-error
    (json-schema-edit-constraint-set-range "0.5..1")
    :type 'user-error)
   (json-schema-edit-constraint-apply)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((type . "integer")
       (maximum . 100)
       (exclusiveMinimum . 0))))
   (json-schema-edit-constraint-set-range "")
   (json-schema-edit-constraint-apply)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((type . "integer"))))))

(ert-deftest json-schema-edit-phase4-test-constraint-draft-04
    ()
  (json-schema-edit-phase4-test--with-constraints
   "{\"$schema\": \"https://json-schema.org/draft-04/schema#\", \"type\": \"number\"}" nil
   (json-schema-edit-constraint-set-range "(0..1)")
   (json-schema-edit-constraint-apply)
   (let-alist
       (json-schema-edit-phase4-test--parse)
     (should
      (equal
       (list .minimum .maximum .exclusiveMinimum .exclusiveMaximum)
       '(0 1 t t))))
   (should
    (equal (json-schema-edit-constraint--range-text) "(0..1)"))))

(ert-deftest json-schema-edit-phase4-test-constraint-values
    ()
  (should
   (equal
    (json-schema-edit-constraint-value-text "a \"b\"" "string")
    "\"a \\\"b\\\"\""))
  (should
   (equal
    (json-schema-edit-constraint-value-text " 3 " "integer")
    "3"))
  (should
   (equal
    (json-schema-edit-constraint-value-text "{\"a\": 1}" nil)
    "{\"a\": 1}"))
  (should-error
   (json-schema-edit-constraint-value-text "3.5" "integer")
   :type 'user-error)
  (should-error
   (json-schema-edit-constraint-value-text "1, 2" nil)
   :type 'user-error)
  (should-error
   (json-schema-edit-constraint-value-text "nope" "boolean")
   :type 'user-error)
  (json-schema-edit-phase4-test--with-constraints
   "{\"type\": \"array\", \"enum\": [[1]]}" nil
   (json-schema-edit-phase4-test--answers
    '("[2]")
    (call-interactively #'json-schema-edit-constraint-enum-add))
   (call-interactively #'json-schema-edit-constraint-unique)
   (json-schema-edit-phase4-test--answers
    '("integer")
    (call-interactively #'json-schema-edit-constraint-items))
   (json-schema-edit-phase4-test--answers
    '("A list")
    (call-interactively #'json-schema-edit-constraint-description))
   (json-schema-edit-constraint-apply)
   (should
    (equal
     (json-schema-edit-phase4-test--parse)
     '((type . "array")
       (enum (1) (2))
       (uniqueItems . t)
       (items (type . "integer"))
       (description . "A list"))))))

(ert-deftest json-schema-edit-phase4-test-constraint-title-and-header
    ()
  (json-schema-edit-phase4-test--with-constraints "{\"type\": \"string\"}" nil
                                                  (json-schema-edit-phase4-test--answers
                                                   '("User name" "A name")
                                                   (call-interactively #'json-schema-edit-constraint-title)
                                                   (call-interactively #'json-schema-edit-constraint-description))
                                                  ;; The header counts the pending changes.
                                                  (should
                                                   (string-search "2 pending"
                                                                  (json-schema-edit-constraint--header)))
                                                  (json-schema-edit-constraint-apply)
                                                  (should
                                                   (equal
                                                    (json-schema-edit-phase4-test--parse)
                                                    '((type . "string")
                                                      (title . "User name")
                                                      (description . "A name"))))))

(ert-deftest json-schema-edit-phase4-test-constraint-undo
    ()
  (json-schema-edit-phase4-test--with-constraints "{\"type\": \"string\"}" nil
                                                  (buffer-enable-undo)
                                                  (undo-boundary)
                                                  (json-schema-edit-constraint-set-range "1..2")
                                                  (json-schema-edit-phase4-test--answers
                                                   '("x")
                                                   (call-interactively #'json-schema-edit-constraint-pattern))
                                                  (json-schema-edit-constraint-apply)
                                                  (undo-boundary)
                                                  (let ((last-command nil))
                                                    (undo))
                                                  (should
                                                   (equal
                                                    (buffer-string)
                                                    "{\"type\": \"string\"}"))))

(ert-deftest json-schema-edit-phase4-test-enum-buffer
    ()
  (json-schema-edit-phase4-test--with-constraints
   "{\"properties\": {\"s\": {\"type\": \"string\", \"enum\": [\"a\", \"b c\"]}}}"
   '("properties" "s")
   (let ((json-schema-edit-enum-return-function #'ignore)
         (source (current-buffer))
         (buf-name "*JSchema Enum: #/properties/s*"))
     (save-window-excursion
       (json-schema-edit-enum-edit)
       (with-current-buffer buf-name
         (should
          (equal (buffer-substring (pos-bol 0) (point-max)) "b c\n"))
         (insert "a\n")
         (should
          (equal
           (mapcar #'caddr (json-schema-edit-enum--problems))
           '("Duplicate value \"a\"")))
         (should-error
          (json-schema-edit-enum-commit)
          :type 'user-error)
         (goto-char (point-max))
         (delete-region (pos-bol 0) (point-max))
         (insert "d \"quoted\"\n")
         (json-schema-edit-enum-commit)))
     (should-not (get-buffer buf-name))
     (with-current-buffer source
       (json-schema-edit-constraint-apply)
       (let-alist
           (json-schema-edit-phase4-test--parse)
         (should
          (equal .properties.s.enum '("a" "b c" "d \"quoted\""))))))))

;;;; Combinator lint

(ert-deftest json-schema-edit-phase4-test-lint-discriminator
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"oneOf\": [
         {\"properties\": {\"kind\": {\"const\": \"a\"}}},
         {\"properties\": {\"kind\": {\"const\": \"a\"}}},
         {\"properties\": {\"other\": {}}}]}"
   (should
    (equal
     (json-schema-edit-phase4-test--messages)
     '("Duplicate discriminating value \"a\""
       "Branch does not fix the discriminating property ‘kind’"
       "Branches #1 and #2 accept the same values, which oneOf rejects")))))

(ert-deftest json-schema-edit-phase4-test-lint-mapping
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"oneOf\": [{\"$ref\": \"#/$defs/Cat\"}, {\"$ref\": \"#/$defs/Dog\"}],
        \"discriminator\": {\"propertyName\": \"pet\",
                            \"mapping\": {\"cat\": \"#/$defs/Dog\", \"dog\": \"#/$defs/Dog\"}},
        \"$defs\": {\"Cat\": {\"properties\": {\"pet\": {\"const\": \"cat\"}}},
                    \"Dog\": {\"properties\": {\"pet\": {\"const\": \"dog\"}}}}}"
   (should
    (equal
     (json-schema-edit-phase4-test--messages)
     '("Mapping ‘cat’ points to a branch whose pet is \"dog\"")))))

(ert-deftest json-schema-edit-phase4-test-lint-overlap
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"oneOf\": [{\"type\": \"string\"}, {\"type\": \"integer\"},
                   {\"type\": \"string\", \"title\": \"again\"}]}"
   (should
    (equal
     (json-schema-edit-phase4-test--messages)
     '("Branches #1 and #3 accept the same values, which oneOf rejects"))))
  (json-schema-edit-phase4-test--with-json
   "{\"anyOf\": [{\"type\": \"string\"}, {\"type\": \"string\"}]}"
   (should (null (json-schema-edit-phase4-test--messages)))))

(ert-deftest json-schema-edit-phase4-test-lint-all-of
    ()
  (json-schema-edit-phase4-test--with-json
   "{\"allOf\": [{\"type\": \"string\"}, {\"type\": [\"integer\", \"null\"]}]}"
   (should
    (equal
     (json-schema-edit-phase4-test--messages)
     '("The types of the allOf branches leave no possible value"))))
  (json-schema-edit-phase4-test--with-json
   "{\"allOf\": [{\"type\": \"number\"}, {\"type\": \"integer\"}]}"
   (should (null (json-schema-edit-phase4-test--messages))))
  (json-schema-edit-phase4-test--with-json
   "{\"properties\": {\"own\": {}},
        \"allOf\": [{\"$ref\": \"#/$defs/Base\"}, {\"properties\": {\"extra\": {}}}],
        \"$defs\": {\"Base\": {\"additionalProperties\": false, \"properties\": {\"id\": {}}}}}"
   (should
    (equal
     (json-schema-edit-phase4-test--messages)
     '("additionalProperties: false rejects own, extra, defined outside this branch")))))

(ert-deftest json-schema-edit-phase4-test-lint-fixtures-clean
    ()
  "The composite fixture is written correctly."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "fixtures/composite.schema.json"
                       (file-name-directory
                        (or load-file-name buffer-file-name
                            (locate-library "json-schema-edit-phase4-test")))))
    (treesit-parser-create 'json)
    (should (null (json-schema-edit-phase4-test--messages)))))

;;;; Every transformation on draft-07 and 2020-12

(ert-deftest json-schema-edit-phase4-test-generated-schemas
    ()
  "Every transformation gives valid JSON that the lint finds no error in.
`task metaschema' checks the same files against the metaschemas."
  (let* ((root
          (expand-file-name ".."
                            (file-name-directory
                             (locate-library "json-schema-edit-phase4-test"))))
         (out (make-temp-file "jse-metaschema" t))
         (default-directory root))
    (unwind-protect
        (progn
          (let ((command-line-args-left (list out)))
            (load
             (expand-file-name "test/metaschema-generate.el" root)
             nil t))
          (let ((files (directory-files out t "\\.json\\'")))
            (should (= (length files) 40))
            (dolist (f files)
              (with-temp-buffer
                (insert-file-contents f)
                (treesit-parser-create 'json)
                (let ((errors
                       (seq-filter
                        (lambda (d) (eq (nth 2 d) :error))
                        (json-schema-edit-lint
                         (json-schema-edit-model-current)))))
                  (should
                   (equal
                    (cons
                     (file-name-nondirectory f)
                     (mapcar #'cadddr errors))
                    (list (file-name-nondirectory f)))))))))
      (delete-directory out t))))

;;;; Menus and outline keys

(ert-deftest json-schema-edit-phase4-test-menus
    ()
  (dolist (cmd
           '(json-schema-edit-dispatch json-schema-edit-combinator-menu
                                       json-schema-edit-constraints-menu))
    (should (commandp cmd)))
  (should
   (eq
    (lookup-key json-schema-edit-mode-map (kbd "C-c ;"))
    #'json-schema-edit-dispatch))
  (should-not (lookup-key json-schema-edit-mode-map (kbd "C-c C-s")))
  (should
   (eq
    (lookup-key json-schema-edit-mode-map (kbd "M-p"))
    #'json-schema-edit-move-property-up))
  (should
   (eq
    (lookup-key json-schema-edit-mode-map (kbd "M-n"))
    #'json-schema-edit-move-property-down))
  (should
   (eq
    (lookup-key json-schema-edit-outline-mode-map (kbd "c"))
    #'json-schema-edit-outline-constraints))
  (should
   (eq
    (lookup-key json-schema-edit-outline-mode-map (kbd "k"))
    #'json-schema-edit-outline-remove-property))
  (should
   (eq
    (lookup-key json-schema-edit-outline-mode-map (kbd "N"))
    #'json-schema-edit-outline-toggle-nullable))
  (should
   (eq
    (lookup-key json-schema-edit-outline-mode-map (kbd "r"))
    #'json-schema-edit-outline-rename))
  (should
   (eq
    (lookup-key json-schema-edit-outline-mode-map (kbd "*"))
    #'json-schema-edit-outline-toggle-required)))

(provide 'json-schema-edit-phase4-test)
;;; json-schema-edit-phase4-test.el ends here
