;;; json-schema-edit-phase2-test.el --- Tests for lint and edits  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `json-schema-edit-lint' and `json-schema-edit-edit'.

;;; Code:

(require 'ert)
(require 'json-schema-edit-mode)

(when-let* ((dir (getenv "TREESIT_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(defconst json-schema-edit-phase2-test--fixtures
  (expand-file-name "fixtures/" (file-name-directory
                                 (or load-file-name buffer-file-name))))

(defmacro json-schema-edit-phase2-test--with-json (text &rest body)
  "Evaluate BODY in a temporary buffer holding the JSON TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (treesit-parser-create 'json)
     (goto-char (point-min))
     ,@body))

(defmacro json-schema-edit-phase2-test--with-copy (name &rest body)
  "Copy the fixtures to a temporary directory and visit NAME there for BODY."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "jse-test" t))
          (buffers nil))
     (unwind-protect
         (progn
           (dolist (f (directory-files json-schema-edit-phase2-test--fixtures t "\\.json\\'"))
             (copy-file f (expand-file-name (file-name-nondirectory f) dir)))
           (let ((buf (find-file-noselect (expand-file-name ,name dir))))
             (push buf buffers)
             (with-current-buffer buf
               (treesit-parser-create 'json)
               (goto-char (point-min))
               ,@body)))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun json-schema-edit-phase2-test--parse ()
  "Parse the current buffer into alists and lists."
  (save-excursion
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun json-schema-edit-phase2-test--goto (text)
  "Move point into the first occurrence of TEXT."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (search-forward text))
  (goto-char (1+ (match-beginning 0))))

(defun json-schema-edit-phase2-test--messages ()
  "Return the lint messages of the current buffer."
  (mapcar (lambda (d) (nth 3 d)) (json-schema-edit-lint)))

;;;; Schema positions

(ert-deftest json-schema-edit-phase2-test-schema-position ()
  (should (json-schema-edit-model-schema-position-p '()))
  (should (json-schema-edit-model-schema-position-p '("properties" "a" "items")))
  (should (json-schema-edit-model-schema-position-p '("items" 0)))
  (should (json-schema-edit-model-schema-position-p '("oneOf" 1 "properties" "x")))
  (should-not (json-schema-edit-model-schema-position-p '("properties")))
  (should-not (json-schema-edit-model-schema-position-p '("examples" 0)))
  (should-not (json-schema-edit-model-schema-position-p '("examples" 0 "properties" "x"))))

;;;; Lint

(ert-deftest json-schema-edit-phase2-test-lint-fixture ()
  (json-schema-edit-phase2-test--with-copy "user.schema.json"
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("Cannot resolve ‘#/$defs/Missing’")))))

(ert-deftest json-schema-edit-phase2-test-lint-required ()
  (json-schema-edit-phase2-test--with-json
      "{\"type\": \"object\", \"required\": [\"a\", \"b\"], \"properties\": {\"a\": {}}}"
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("‘b’ is required but not in properties")))))

(ert-deftest json-schema-edit-phase2-test-lint-type-keywords ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {
         \"s\": {\"type\": \"string\", \"minimum\": 1, \"maxLength\": 3},
         \"i\": {\"type\": \"integer\", \"minimum\": 1},
         \"n\": {\"type\": [\"string\", \"null\"], \"items\": {}}},
        \"examples\": [{\"type\": \"string\", \"minimum\": 1}]}"
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("‘minimum’ has no effect on type string"
                     "‘items’ has no effect on type string | null")))))

(ert-deftest json-schema-edit-phase2-test-lint-unused ()
  (json-schema-edit-phase2-test--with-json
      "{\"$defs\": {\"A\": {\"properties\": {\"x\": {}}},
                    \"B\": {\"$ref\": \"#/$defs/A/properties/x\"},
                    \"C\": {\"$anchor\": \"c\"}}}"
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("Definition ‘B’ is not used")))))

(ert-deftest json-schema-edit-phase2-test-lint-unused-external ()
  "Definitions used only from other files are not reported."
  (json-schema-edit-phase2-test--with-copy "common.json"
    ;; Make Timestamps referenced only from the other files.
    (json-schema-edit-phase2-test--goto "#/$defs/Timestamps")
    (delete-region (point) (+ (point) (length "/$defs/Timestamps")))
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("Definition ‘Self’ is not used")))
    (let ((json-schema-edit-lint-unused-definitions 'file))
      (should (equal (json-schema-edit-phase2-test--messages)
                     '("Definition ‘Timestamps’ is not used"
                       "Definition ‘Self’ is not used"))))))

(ert-deftest json-schema-edit-phase2-test-flymake ()
  (json-schema-edit-phase2-test--with-copy "user.schema.json"
    (let (diags)
      (json-schema-edit-lint-flymake (lambda (d &rest _) (setq diags d)))
      (should (= (length diags) 1))
      (should (eq (flymake-diagnostic-type (car diags)) :error)))))

;;;; Adding properties

(ert-deftest json-schema-edit-phase2-test-new-schema ()
  (dolist (case '(("2020-12" . "https://json-schema.org/draft/2020-12/schema")
                  ("draft-07" . "https://json-schema.org/draft-07/schema#")))
    (save-window-excursion
      (json-schema-edit-new (car case) "Person")
      (unwind-protect
          (progn
            (should json-schema-edit-mode)
            (should (equal (json-schema-edit-phase2-test--parse)
                           `((\$schema . ,(cdr case)) (title . "Person")
                             (type . "object") (properties))))
            (json-schema-edit-add-property nil "name" "string" nil)
            (should (json-schema-edit-model-get
                     (json-schema-edit-model-node-at-pointer '("properties")) "name")))
        (kill-buffer (current-buffer)))))
  (should-error (json-schema-edit-new "2019-09" "X") :type 'user-error))

(ert-deftest json-schema-edit-phase2-test-add-property-multiline ()
  (json-schema-edit-phase2-test--with-copy "user.schema.json"
    (json-schema-edit-phase2-test--goto "\"zip\": {")
    (json-schema-edit-add-property (json-schema-edit-edit--owner-pointer)
                                   "country" "string" t)
    (let-alist (json-schema-edit-phase2-test--parse)
      (should (equal .$defs.Address.properties.country '((type . "string"))))
      (should (equal .$defs.Address.required '("zip" "country"))))
    (should (looking-at-p "\"country\""))
    (json-schema-edit-phase2-test--goto "\"city\"")
    (forward-line 1)
    (should (looking-at-p "        \"country\": { \"type\": \"string\" }$"))
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("Cannot resolve ‘#/$defs/Missing’")))))

(ert-deftest json-schema-edit-phase2-test-add-property-creates-members ()
  (json-schema-edit-phase2-test--with-json "{\n  \"type\": \"object\"\n}"
    (json-schema-edit-add-property nil "id" "#/$defs/Id" t)
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((type . "object")
                     (properties (id ($ref . "#/$defs/Id")))
                     (required "id"))))
    (should (equal (buffer-string)
                   "{\n  \"type\": \"object\",\n  \"properties\": {\n    \"id\": { \"$ref\": \"#/$defs/Id\" }\n  },\n  \"required\": [\"id\"]\n}"))))

(ert-deftest json-schema-edit-phase2-test-add-property-inline-and-empty ()
  (json-schema-edit-phase2-test--with-json "{\"properties\": {}, \"required\": []}"
    (json-schema-edit-add-property nil "a" "string" t)
    (json-schema-edit-add-property nil "b" "integer" nil)
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a (type . "string")) (b (type . "integer")))
                     (required "a"))))
    (should-error (json-schema-edit-add-property nil "a" "string" nil)
                  :type 'user-error)))

(ert-deftest json-schema-edit-phase2-test-add-object-property-nested ()
  (json-schema-edit-phase2-test--with-json "{\"type\": \"object\"}"
    (json-schema-edit-add-property nil "child" "object" nil)
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((type . "object")
                     (properties (child (type . "object") (properties))))))
    (json-schema-edit-add-property '("properties" "child") "leaf" "string" nil)
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((type . "object")
                     (properties (child (type . "object")
                                        (properties (leaf (type . "string"))))))))))

(ert-deftest json-schema-edit-phase2-test-add-property-owner ()
  (json-schema-edit-phase2-test--with-copy "user.schema.json"
    ;; On a non-object property, the property goes next to it.
    (json-schema-edit-phase2-test--goto "\"minLength\"")
    (should (null (json-schema-edit-edit--owner-pointer)))
    (json-schema-edit-phase2-test--goto "\"city\": {")
    (should (equal (json-schema-edit-edit--owner-pointer) '("$defs" "Address")))))

(ert-deftest json-schema-edit-phase2-test-add-property-undo ()
  (json-schema-edit-phase2-test--with-json "{\"properties\": {}}"
    (buffer-enable-undo)
    (undo-boundary)
    (json-schema-edit-add-property nil "a" "string" t)
    (undo-boundary)
    (let ((last-command nil)) (undo))
    (should (equal (buffer-string) "{\"properties\": {}}"))))

;;;; Renaming properties

(ert-deftest json-schema-edit-phase2-test-rename-property ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}, \"aa\": {}},
        \"required\": [\"a\", \"aa\"],
        \"dependentRequired\": {\"a\": [\"b\"], \"b\": [\"a\"]},
        \"examples\": [{\"a\": 1}]}"
    (json-schema-edit-phase2-test--goto "\"a\": {}")
    (should (equal (json-schema-edit-edit--property-pointer) '("properties" "a")))
    (json-schema-edit-rename-property '("properties" "a") "name")
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (name) (b) (aa))
                     (required "name" "aa")
                     (dependentRequired (name "b") (b "name"))
                     (examples ((a . 1))))))
    (should (null (json-schema-edit-phase2-test--messages)))
    (should-error (json-schema-edit-rename-property '("properties" "b") "aa")
                  :type 'user-error)))

(ert-deftest json-schema-edit-phase2-test-rename-property-draft-07 ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}},
        \"dependencies\": {\"a\": [\"b\"], \"b\": {\"required\": [\"a\"]}}}"
    (json-schema-edit-rename-property '("properties" "b") "c")
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a) (c))
                     (dependencies (a "c") (c (required "a"))))))))

(ert-deftest json-schema-edit-phase2-test-rename-property-dependent-schemas ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}},
        \"dependentSchemas\": {\"b\": {\"required\": [\"a\"]}}}"
    (json-schema-edit-rename-property '("properties" "b") "c")
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a) (c))
                     (dependentSchemas (c (required "a"))))))))

;;;; Removing properties

(ert-deftest json-schema-edit-phase2-test-remove-property ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}},
        \"required\": [\"a\", \"b\"],
        \"dependentRequired\": {\"a\": [\"b\"]},
        \"dependentSchemas\": {\"b\": {}}}"
    (json-schema-edit-remove-property '("properties" "b"))
    ;; required is cleaned; dependentRequired and dependentSchemas are
    ;; dropped once nothing refers to them anymore.
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a))
                     (required "a")))))
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}}, \"required\": [\"a\"]}"
    (json-schema-edit-remove-property '("properties" "a"))
    ;; An empty required is removed.
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties)))))
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}}}"
    ;; Interactively, the removal is confirmed.
    (json-schema-edit-phase2-test--goto "\"b\": {}")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (call-interactively #'json-schema-edit-remove-property))
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a))))))
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}}}"
    ;; Declining the confirmation leaves the buffer untouched.
    (json-schema-edit-phase2-test--goto "\"b\": {}")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (call-interactively #'json-schema-edit-remove-property)
                    :type 'user-error))
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a) (b)))))))

;;;; Moving properties

(ert-deftest json-schema-edit-phase2-test-move-property ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"a\": {}, \"b\": {}, \"c\": {}}}"
    (json-schema-edit-move-property-down '("properties" "a"))
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (b) (a) (c)))))
    (json-schema-edit-move-property-up '("properties" "a"))
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (a) (b) (c)))))
    (should-error (json-schema-edit-move-property-up '("properties" "a"))
                  :type 'user-error)
    (should-error (json-schema-edit-move-property-down '("properties" "c"))
                  :type 'user-error)
    ;; Only the properties of an object can be moved.
    (should-error (json-schema-edit-edit--move-property '("oneOf" 0) 1)
                  :type 'user-error)))

(ert-deftest json-schema-edit-phase2-test-move-property-nested ()
  (json-schema-edit-phase2-test--with-json
      "{\"properties\": {\"o\": {\"properties\": {\"x\": {}, \"y\": {}}}}}"
    (json-schema-edit-move-property-down '("properties" "o" "properties" "x"))
    (should (equal (json-schema-edit-phase2-test--parse)
                   '((properties (o (properties (y) (x)))))))))

;;;; Renaming definitions

(ert-deftest json-schema-edit-phase2-test-rename-definition ()
  (json-schema-edit-phase2-test--with-copy "user.schema.json"
    (json-schema-edit-phase2-test--goto "#/$defs/Address")
    (should (equal (json-schema-edit-edit--definition-pointer) '($defs "Address")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (json-schema-edit-rename-definition '($defs "Address") "PostalAddress"))
    (let-alist (json-schema-edit-phase2-test--parse)
      (should .$defs.PostalAddress)
      (should-not .$defs.Address)
      (should (equal .properties.address '(($ref . "#/$defs/PostalAddress"))))
      (should (equal .properties.home '(($ref . "#/$defs/PostalAddress")))))
    (should (equal (json-schema-edit-phase2-test--messages)
                   '("Cannot resolve ‘#/$defs/Missing’")))
    ;; other.json was updated in its buffer but not saved.
    (with-current-buffer (find-buffer-visiting
                          (expand-file-name "other.json" default-directory))
      (should (buffer-modified-p))
      (let-alist (json-schema-edit-phase2-test--parse)
        (should (equal .definitions.UsesAddress
                       '(($ref . "user.schema.json#/$defs/PostalAddress"))))))))

(ert-deftest json-schema-edit-phase2-test-rename-definition-nested-refs ()
  (json-schema-edit-phase2-test--with-json
      "{\"$defs\": {\"a/b\": {\"properties\": {\"x\": {}}}},
        \"properties\": {\"p\": {\"$ref\": \"#/$defs/a~1b/properties/x\"},
                         \"q\": {\"$ref\": \"#/$defs/a~1bc\"}}}"
    (json-schema-edit-rename-definition '($defs "a/b") "c~d")
    (let-alist (json-schema-edit-phase2-test--parse)
      (should (equal .properties.p '(($ref . "#/$defs/c~0d/properties/x"))))
      (should (equal .properties.q '(($ref . "#/$defs/a~1bc")))))))

(ert-deftest json-schema-edit-phase2-test-rename-definition-decline-others ()
  (json-schema-edit-phase2-test--with-copy "common.json"
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (json-schema-edit-rename-definition '($defs "Timestamps") "Times"))
    (let-alist (json-schema-edit-phase2-test--parse)
      (should (equal .$defs.Self '(($ref . "#/$defs/Times")))))
    (should-not (find-buffer-visiting (expand-file-name "other.json" default-directory)))))

(provide 'json-schema-edit-phase2-test)
;;; json-schema-edit-phase2-test.el ends here
