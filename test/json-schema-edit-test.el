;;; json-schema-edit-test.el --- Tests for json-schema-edit-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `task test'.  The json grammar is looked up in
;; `treesit-extra-load-path' and in $TREESIT_GRAMMAR_DIR.

;;; Code:

(require 'ert)
(require 'json-schema-edit-mode)

(when-let* ((dir (getenv "TREESIT_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(defconst json-schema-edit-test--fixtures
  (expand-file-name "fixtures/" (file-name-directory
                                 (or load-file-name buffer-file-name))))

(defmacro json-schema-edit-test--with-fixture (name &rest body)
  "Visit fixture NAME with `json-schema-edit-mode' and evaluate BODY."
  (declare (indent 1))
  `(let ((buf (find-file-noselect
               (expand-file-name ,name json-schema-edit-test--fixtures))))
     (unwind-protect
         (with-current-buffer buf
           (treesit-parser-create 'json)
           (json-schema-edit-mode 1)
           (goto-char (point-min))
           ,@body)
       (kill-buffer buf))))

(defun json-schema-edit-test--goto (text &optional nth)
  "Move point into the NTH (default 1) occurrence of TEXT."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (search-forward text nil nil (or nth 1)))
  (goto-char (1+ (match-beginning 0))))

(defun json-schema-edit-test--pointer-at-target (ref)
  "Return the pointer fragment that REF resolves to."
  (when-let* ((res (json-schema-edit-model-resolve (json-schema-edit-model-current) ref)))
    (json-schema-edit-model-pointer-fragment (map-elt res :pointer))))

;;;; Keyword types

(ert-deftest json-schema-edit-test-keyword-types ()
  (should (eq (caar json-schema-edit-model--keyword-types) 'minLength))
  (should (eq (cdar json-schema-edit-model--keyword-types) 'string))
  (should (json-schema-edit-model-keyword-applies-p "minLength" '("string")))
  (should-not (json-schema-edit-model-keyword-applies-p "minLength" '("number")))
  (should (json-schema-edit-model-keyword-applies-p "minimum" '("integer")))
  (should (json-schema-edit-model-keyword-applies-p "title" '("string"))))

(ert-deftest json-schema-edit-test-combinators ()
  (should (equal json-schema-edit-model--combinators '(oneOf anyOf allOf))))

;;;; Pointer

(ert-deftest json-schema-edit-test-pointer-roundtrip ()
  (should (equal (json-schema-edit-model-pointer-encode '("a/b" "c~d" 0))
                 "/a~1b/c~0d/0"))
  (should (equal (json-schema-edit-model-pointer-decode "/a~1b/c~0d/0")
                 '("a/b" "c~d" "0")))
  (should (equal (json-schema-edit-model-pointer-decode "/%24defs/x%20y")
                 '("$defs" "x y")))
  (should (null (json-schema-edit-model-pointer-decode ""))))

(ert-deftest json-schema-edit-test-pointer-at ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (json-schema-edit-test--goto "\"zip\": {")
    (should (equal (json-schema-edit-model-pointer-at (json-schema-edit-model-current) (point))
                   '($defs "Address" properties "zip")))
    (json-schema-edit-test--goto "\"name\"]")
    (should (equal (json-schema-edit-model-pointer-at (json-schema-edit-model-current) (point)) '(required 1)))))

(ert-deftest json-schema-edit-test-node-at-pointer ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (let ((node (json-schema-edit-model-node-at-pointer
                 (json-schema-edit-model-current) '("properties" "contact" "oneOf" "1"))))
      (should (equal (json-schema-edit-model-get-string node "$ref")
                     "#/$defs/Phone")))))

(ert-deftest json-schema-edit-test-schema-version ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (should (eq (json-schema-edit-model-schema-version (json-schema-edit-model-current)) '2020-12)))
  (json-schema-edit-test--with-fixture "other.json"
    (should (eq (json-schema-edit-model-schema-version (json-schema-edit-model-current)) 'draft-07))))

(ert-deftest json-schema-edit-test-schema-version-full-uri ()
  (dolist (case '(("https://json-schema.org/draft/2020-12/schema" . 2020-12)
                  ("https://json-schema.org/draft-04/schema#" . draft-04)
                  ("https://example.com/draft-07/schema#" . nil)
                  ("https://json-schema.org/draft/2020-12/schema/extra" . nil)))
    (with-temp-buffer
      (insert (format "{\"$schema\": \"%s\"}" (car case)))
      (treesit-parser-create 'json)
      (should (eq (json-schema-edit-model-schema-version (json-schema-edit-model-current)) (cdr case)))))
  (with-temp-buffer
    (insert "{\"$schema\": \"https://example.com/my-schema\"}")
    (treesit-parser-create 'json)
    (let ((json-schema-edit-schema-versions
           '(("https://example.com/my-schema" . draft-07))))
      (should (eq (json-schema-edit-model-schema-version (json-schema-edit-model-current)) 'draft-07)))))

;;;; Model

(ert-deftest json-schema-edit-test-model-is-data ()
  "Queries work on a model without its buffer."
  (let* ((model (json-schema-edit-model-from-string
                 "{\"$defs\": {\"A\": {\"$anchor\": \"a\", \"type\": \"string\"}},
  \"properties\": {\"x\": {\"$ref\": \"#/$defs/A\"}}}"))
         (x (json-schema-edit-model-node-at-pointer model '(properties "x"))))
    (should (equal (json-schema-edit-model-node-pointer x) '(properties "x")))
    (should (equal (json-schema-edit-model-get-string x '$ref) "#/$defs/A"))
    (should (equal (mapcar #'json-schema-edit-model-ref-value
                           (json-schema-edit-model-refs model))
                   '("#/$defs/A")))
    (should (equal (json-schema-edit-model-ref-pointer
                    (car (json-schema-edit-model-refs model)))
                   '(properties "x")))
    (should (equal (map-elt (json-schema-edit-model-resolve model "#a") :pointer)
                   '($defs "A")))
    (should (equal (json-schema-edit-header-type-string
                    (map-elt (json-schema-edit-model-resolve model "#/$defs/A") :node))
                   "string"))))

(ert-deftest json-schema-edit-test-model-keyword-keys ()
  "Keys of schema objects are keyword symbols; names and data are strings."
  (let* ((model (json-schema-edit-model-from-string
                 "{\"properties\": {\"type\": {\"type\": \"string\",
  \"examples\": [{\"$ref\": \"x\"}]}}}"))
         (prop (json-schema-edit-model-node-at-pointer model '(properties "type"))))
    (should (equal (mapcar #'json-schema-edit-model-node-key
                           (json-schema-edit-model-members prop))
                   '(type examples)))
    (should (equal (json-schema-edit-model-node-pointer
                    (json-schema-edit-model-node-at-pointer
                     model '(properties "type" examples 0 "$ref")))
                   '(properties "type" examples 0 "$ref")))
    ;; A "$ref" inside `examples' is data, not a reference.
    (should-not (json-schema-edit-model-refs model))))

(ert-deftest json-schema-edit-test-canonical-pointer ()
  (should (equal (json-schema-edit-model-canonical-pointer
                  '("properties" "type" "items" "0" "oneOf" "1" "enum" "0"))
                 '(properties "type" items 0 oneOf 1 enum "0")))
  (should (equal (json-schema-edit-model-canonical-pointer
                  '($defs name properties "a"))
                 '($defs "name" properties "a")))
  (should (equal (json-schema-edit-model-canonical-pointer
                  (json-schema-edit-model-pointer-decode "/$defs/A/anyOf/0"))
                 '($defs "A" anyOf 0))))

(ert-deftest json-schema-edit-test-model-pointer-at ()
  (let* ((text "{\n  \"a\": [1, {\"b\": true}]\n}")
         (model (json-schema-edit-model-from-string text))
         (pos (lambda (s) (1+ (string-search s text)))))
    (should (equal (json-schema-edit-model-pointer-at model (funcall pos "\"a\"")) '(a)))
    (should (equal (json-schema-edit-model-pointer-at model (funcall pos "true")) '(a 1 "b")))
    ;; In whitespace, the node after it.
    (should (equal (json-schema-edit-model-pointer-at model 3) '(a)))
    (should (equal (json-schema-edit-model-line-column model (funcall pos "true"))
                   '(2 . 17)))))

(ert-deftest json-schema-edit-test-model-resolve-with-loader ()
  (let* ((other (json-schema-edit-model-from-string "{\"$defs\": {\"B\": {}}}"))
         (model (json-schema-edit-model-from-string "{\"$ref\": \"b.json#/$defs/B\"}"))
         (res (json-schema-edit-model-resolve
               model "b.json#/$defs/B"
               (lambda (file) (and (equal (file-name-nondirectory file) "b.json") other)))))
    (should (eq (map-elt res :model) other))
    (should (equal (map-elt res :pointer) '($defs "B")))))

;;;; Resolution

(ert-deftest json-schema-edit-test-resolve-local ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (should (equal (json-schema-edit-test--pointer-at-target "#/$defs/Address")
                   "#/$defs/Address"))
    (should (equal (json-schema-edit-test--pointer-at-target "#/$defs/a~1b")
                   "#/$defs/a~1b"))
    (should (equal (json-schema-edit-test--pointer-at-target "#tag")
                   "#/$defs/Tag"))
    (should (equal (json-schema-edit-test--pointer-at-target "#") "#"))
    (should (null (json-schema-edit-model-resolve (json-schema-edit-model-current) "#/$defs/Missing")))
    (should (null (json-schema-edit-model-resolve (json-schema-edit-model-current) "https://example.com/x.json")))))

(ert-deftest json-schema-edit-test-resolve-external ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (let ((res (json-schema-edit-model-resolve (json-schema-edit-model-current) "common.json#/$defs/Timestamps")))
      (should (equal (map-elt res :file)
                     (expand-file-name "common.json" json-schema-edit-test--fixtures)))
      (should (equal (map-elt res :pointer) '($defs "Timestamps"))))))

(ert-deftest json-schema-edit-test-every-ref-resolves ()
  "Every $ref in the fixtures except the intentionally broken one resolves."
  (dolist (file '("user.schema.json" "common.json" "other.json"))
    (json-schema-edit-test--with-fixture file
      (dolist (ref (json-schema-edit-model-refs (json-schema-edit-model-current)))
        (let ((value (json-schema-edit-model-ref-value ref)))
          (unless (equal value "#/$defs/Missing")
            (should (json-schema-edit-model-resolve (json-schema-edit-model-current) value))))))))

;;;; xref

(ert-deftest json-schema-edit-test-xref-definition ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (json-schema-edit-test--goto "#/$defs/Address")
    (let* ((id (xref-backend-identifier-at-point 'json-schema-edit))
           (defs (xref-backend-definitions 'json-schema-edit id))
           (loc (xref-item-location (car defs))))
      (should (equal id "#/$defs/Address"))
      (should (= (length defs) 1))
      (goto-char (xref-location-marker loc))
      (should (equal (json-schema-edit-model-pointer-at (json-schema-edit-model-current) (point)) '($defs "Address"))))))

(ert-deftest json-schema-edit-test-xref-external-definition ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (json-schema-edit-test--goto "common.json#")
    (let* ((id (xref-backend-identifier-at-point 'json-schema-edit))
           (loc (xref-item-location
                 (car (xref-backend-definitions 'json-schema-edit id)))))
      (should (equal (file-name-nondirectory (xref-location-group loc)) "common.json"))
      (should (= (xref-location-line loc) 4)))))

(ert-deftest json-schema-edit-test-xref-references ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (json-schema-edit-test--goto "\"Address\": {")
    (let* ((id (xref-backend-identifier-at-point 'json-schema-edit))
           (refs (xref-backend-references 'json-schema-edit id))
           (groups (mapcar (lambda (x)
                             (file-name-nondirectory
                              (xref-location-group (xref-item-location x))))
                           refs)))
      (should (equal id "#/$defs/Address"))
      ;; address, home in this file and UsesAddress in other.json.
      (should (= (length refs) 3))
      (should (equal (seq-count (lambda (g) (equal g "other.json")) groups) 1)))))

(ert-deftest json-schema-edit-test-xref-references-from-other-file ()
  (json-schema-edit-test--with-fixture "common.json"
    (json-schema-edit-test--goto "\"Timestamps\": {")
    (let ((refs (xref-backend-references
                 'json-schema-edit
                 (xref-backend-identifier-at-point 'json-schema-edit))))
      ;; Self in common.json, audit in user.schema.json, UsesTimestamps in other.json.
      (should (= (length refs) 3)))))

;;;; eldoc

(ert-deftest json-schema-edit-test-eldoc ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (let (doc)
      (json-schema-edit-test--goto "#/$defs/Address")
      (json-schema-edit-xref-eldoc-function (lambda (s &rest _) (setq doc s)))
      (should (equal doc "object — Postal address"))
      (json-schema-edit-test--goto "#/$defs/Missing")
      (json-schema-edit-xref-eldoc-function (lambda (s &rest _) (setq doc s)))
      (should (equal (substring-no-properties doc) "unresolved")))))

;;;; Breadcrumb

(ert-deftest json-schema-edit-test-breadcrumb ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (json-schema-edit-test--goto "\"zip\": {")
    (should (equal (json-schema-edit-header-breadcrumb) "Address › zip : string *"))
    (json-schema-edit-test--goto "\"minLength\"")
    (should (equal (json-schema-edit-header-breadcrumb) "User › name : string *"))
    (json-schema-edit-test--goto "\"address\": { \"type\"")
    (should (equal (json-schema-edit-header-breadcrumb)
                   "User › contact › ‹oneOf: email› › address : string"))
    (json-schema-edit-test--goto "#tag")
    (should (equal (json-schema-edit-header-breadcrumb) "User › tags › [] : → #tag"))
    (let ((json-schema-edit-breadcrumb-style 'schema))
      (should (equal (json-schema-edit-header-breadcrumb) "#/properties/tags/items")))))

(ert-deftest json-schema-edit-test-mode-maybe ()
  (json-schema-edit-test--with-fixture "common.json"
    (json-schema-edit-mode -1)
    (json-schema-edit-mode-maybe)
    (should json-schema-edit-mode)
    (should (memq #'json-schema-edit-xref-backend xref-backend-functions))
    (json-schema-edit-mode -1)
    (should-not (memq #'json-schema-edit-xref-backend xref-backend-functions))))

(ert-deftest json-schema-edit-test-mode-maybe-root-combinator ()
  (with-temp-buffer
    (insert "{\"oneOf\": [{\"type\": \"string\"}, {\"type\": \"number\"}]}")
    (treesit-parser-create 'json)
    (json-schema-edit-mode-maybe)
    (should json-schema-edit-mode)))

(provide 'json-schema-edit-test)
;;; json-schema-edit-test.el ends here
