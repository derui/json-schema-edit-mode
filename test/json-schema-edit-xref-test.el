;;; json-schema-edit-xref-test.el --- Tests for xref and eldoc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `json-schema-edit-xref'.

;;; Code:

(require 'ert)
(require 'json-schema-edit-mode)

(when-let* ((dir (getenv "TREESIT_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(defconst json-schema-edit-xref-test--fixtures
  (expand-file-name "fixtures/" (file-name-directory
                                 (or load-file-name buffer-file-name))))

(defmacro json-schema-edit-xref-test--with-fixture (name &rest body)
  "Visit fixture NAME with `json-schema-edit-mode' and evaluate BODY."
  (declare (indent 1))
  `(let ((buf (find-file-noselect
               (expand-file-name ,name json-schema-edit-xref-test--fixtures))))
     (unwind-protect
         (with-current-buffer buf
           (treesit-parser-create 'json)
           (json-schema-edit-mode 1)
           (goto-char (point-min))
           ,@body)
       (kill-buffer buf))))

(defun json-schema-edit-xref-test--goto (text &optional nth)
  "Move point into the NTH (default 1) occurrence of TEXT."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (search-forward text nil nil (or nth 1)))
  (goto-char (1+ (match-beginning 0))))

;;;; xref

(ert-deftest json-schema-edit-xref-test-definition ()
  (json-schema-edit-xref-test--with-fixture "user.schema.json"
    (json-schema-edit-xref-test--goto "#/$defs/Address")
    (let* ((id (xref-backend-identifier-at-point 'json-schema-edit))
           (defs (xref-backend-definitions 'json-schema-edit id))
           (loc (xref-item-location (car defs))))
      (should (equal id "#/$defs/Address"))
      (should (= (length defs) 1))
      (goto-char (xref-location-marker loc))
      (should (equal (json-schema-edit-model-pointer-at (json-schema-edit-model-current) (point)) '($defs "Address"))))))

(ert-deftest json-schema-edit-xref-test-external-definition ()
  (json-schema-edit-xref-test--with-fixture "user.schema.json"
    (json-schema-edit-xref-test--goto "common.json#")
    (let* ((id (xref-backend-identifier-at-point 'json-schema-edit))
           (loc (xref-item-location
                 (car (xref-backend-definitions 'json-schema-edit id)))))
      (should (equal (file-name-nondirectory (xref-location-group loc)) "common.json"))
      (should (= (xref-location-line loc) 4)))))

(ert-deftest json-schema-edit-xref-test-references ()
  (json-schema-edit-xref-test--with-fixture "user.schema.json"
    (json-schema-edit-xref-test--goto "\"Address\": {")
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

(ert-deftest json-schema-edit-xref-test-references-from-other-file ()
  (json-schema-edit-xref-test--with-fixture "common.json"
    (json-schema-edit-xref-test--goto "\"Timestamps\": {")
    (let ((refs (xref-backend-references
                 'json-schema-edit
                 (xref-backend-identifier-at-point 'json-schema-edit))))
      ;; Self in common.json, audit in user.schema.json, UsesTimestamps in other.json.
      (should (= (length refs) 3)))))

;;;; eldoc

(ert-deftest json-schema-edit-xref-test-eldoc ()
  (json-schema-edit-xref-test--with-fixture "user.schema.json"
    (let (doc)
      (json-schema-edit-xref-test--goto "#/$defs/Address")
      (json-schema-edit-xref-eldoc-function (lambda (s &rest _) (setq doc s)))
      (should (equal doc "object — Postal address"))
      (json-schema-edit-xref-test--goto "#/$defs/Missing")
      (json-schema-edit-xref-eldoc-function (lambda (s &rest _) (setq doc s)))
      (should (equal (substring-no-properties doc) "unresolved")))))

;;;; Summary

(ert-deftest json-schema-edit-xref-test-summary ()
  (let ((model (json-schema-edit-model-from-string
                "{\"$defs\": {\"A\": {\"type\": \"string\"},
  \"B\": {\"type\": \"integer\", \"description\": \"A count\"}}}")))
    (should (equal (json-schema-edit-xref--summary model '($defs "A"))
                   "string"))
    (should (equal (json-schema-edit-xref--summary model '($defs "B"))
                   "integer — A count"))))

(provide 'json-schema-edit-xref-test)
;;; json-schema-edit-xref-test.el ends here
