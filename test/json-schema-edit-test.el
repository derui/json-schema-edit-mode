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
  (when-let* ((res (json-schema-edit-model-resolve ref)))
    (json-schema-edit-model-pointer-fragment (map-elt res :pointer))))

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
    (should (equal (json-schema-edit-model-pointer-at)
                   '("$defs" "Address" "properties" "zip")))
    (json-schema-edit-test--goto "\"name\"]")
    (should (equal (json-schema-edit-model-pointer-at) '("required" 1)))))

(ert-deftest json-schema-edit-test-node-at-pointer ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (let ((node (json-schema-edit-model-node-at-pointer
                 '("properties" "contact" "oneOf" "1"))))
      (should (equal (json-schema-edit-model-get-string node "$ref")
                     "#/$defs/Phone")))))

(ert-deftest json-schema-edit-test-draft ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (should (eq (json-schema-edit-model-draft) '2020-12)))
  (json-schema-edit-test--with-fixture "other.json"
    (should (eq (json-schema-edit-model-draft) 'draft-07))))

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
    (should (null (json-schema-edit-model-resolve "#/$defs/Missing")))
    (should (null (json-schema-edit-model-resolve "https://example.com/x.json")))))

(ert-deftest json-schema-edit-test-resolve-external ()
  (json-schema-edit-test--with-fixture "user.schema.json"
    (let ((res (json-schema-edit-model-resolve "common.json#/$defs/Timestamps")))
      (should (equal (map-elt res :file)
                     (expand-file-name "common.json" json-schema-edit-test--fixtures)))
      (should (equal (map-elt res :pointer) '("$defs" "Timestamps"))))))

(ert-deftest json-schema-edit-test-every-ref-resolves ()
  "Every $ref in the fixtures except the intentionally broken one resolves."
  (dolist (file '("user.schema.json" "common.json" "other.json"))
    (json-schema-edit-test--with-fixture file
      (dolist (ref (json-schema-edit-model-refs))
        (let ((value (json-schema-edit-model-ref-value ref)))
          (unless (equal value "#/$defs/Missing")
            (should (json-schema-edit-model-resolve value))))))))

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
      (should (equal (json-schema-edit-model-pointer-at) '("$defs" "Address"))))))

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
    (should (equal (json-schema-edit-breadcrumb) "Address › zip : string *"))
    (json-schema-edit-test--goto "\"minLength\"")
    (should (equal (json-schema-edit-breadcrumb) "User › name : string *"))
    (json-schema-edit-test--goto "\"address\": { \"type\"")
    (should (equal (json-schema-edit-breadcrumb)
                   "User › contact › ‹oneOf: email› › address : string"))
    (json-schema-edit-test--goto "#tag")
    (should (equal (json-schema-edit-breadcrumb) "User › tags › [] : → #tag"))
    (let ((json-schema-edit-breadcrumb-style 'schema))
      (should (equal (json-schema-edit-breadcrumb) "#/properties/tags/items")))))

(ert-deftest json-schema-edit-test-mode-maybe ()
  (json-schema-edit-test--with-fixture "common.json"
    (json-schema-edit-mode -1)
    (json-schema-edit-mode-maybe)
    (should json-schema-edit-mode)
    (should (memq #'json-schema-edit-xref-backend xref-backend-functions))
    (json-schema-edit-mode -1)
    (should-not (memq #'json-schema-edit-xref-backend xref-backend-functions))))

(provide 'json-schema-edit-test)
;;; json-schema-edit-test.el ends here
