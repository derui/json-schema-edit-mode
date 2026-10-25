;;; metaschema-generate.el --- Write transformed schemas for metaschema checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: emacs -Q --batch -L . -l test/metaschema-generate.el OUT-DIR
;;
;; Every transformation is applied, one at a time, to each base schema
;; in test/metaschema/, and the result is written to
;; OUT-DIR/<draft>--<transformation>.json.  `task metaschema' then
;; checks every file against its metaschema.

;;; Code:

(require 'json-schema-edit-mode)

(when-let* ((dir (getenv "TREESIT_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(defvar metaschema-generate--answers nil
  "Answers given to the minibuffer questions of a transformation.")

(defun metaschema-generate--answer (&rest _)
  "Return the next answer."
  (pop metaschema-generate--answers))

(defun metaschema-generate--range (pointer range)
  "Return a function setting RANGE on the schema at POINTER."
  (lambda ()
    (let ((json-schema-edit-constraint--buffer (current-buffer))
          (json-schema-edit-constraint--pointer pointer)
          (json-schema-edit-constraint--pending nil))
      (json-schema-edit-constraint-set-range range)
      (json-schema-edit-constraint-apply))))

(defun metaschema-generate--transformations (defs)
  "Return (NAME ANSWERS FUNCTION) for every transformation.
DEFS is the definitions keyword of the base schema."
  `(("nullable-type" nil ,(lambda () (json-schema-edit-toggle-nullable '("properties" "note"))))
    ("nullable-ref" nil ,(lambda () (json-schema-edit-toggle-nullable '("properties" "customer"))))
    ("array" nil ,(lambda () (json-schema-edit-wrap-array '("properties" "note"))))
    ("wrap-oneOf" nil ,(lambda () (json-schema-edit-wrap-combinator '("properties" "count") 'oneOf)))
    ("extract" nil ,(lambda () (json-schema-edit-extract-definition '("properties" "payment") "Payment")))
    ("inline" (t) ,(lambda () (json-schema-edit-inline-ref '("properties" "customer"))))
    ("add-branch" ("bank") ,(lambda () (json-schema-edit-add-branch '("properties" "payment") 'oneOf)))
    ("remove-branch" nil ,(lambda () (json-schema-edit-remove-branch '("properties" "payment") 'oneOf 1)))
    ("duplicate-branch" nil ,(lambda () (json-schema-edit-duplicate-branch '("properties" "shipping") 'anyOf 0)))
    ("move-branch" nil ,(lambda () (json-schema-edit-move-branch-down '("properties" "shipping") 'anyOf 0)))
    ("switch" nil ,(lambda () (json-schema-edit-switch-combinator '("properties" "shipping") 'anyOf 'oneOf)))
    ("discriminator" ("home" "store") ,(lambda () (json-schema-edit-set-discriminator '("properties" "shipping") 'anyOf "kind")))
    ("factor" nil ,(lambda () (json-schema-edit-factor-common-properties '("properties" "payment") 'oneOf)))
    ("add-property" ("string" t) ,(lambda () (json-schema-edit-add-property (list defs "Customer") "email"
                                                                              (pop metaschema-generate--answers)
                                                                              (pop metaschema-generate--answers))))
    ("required" nil ,(lambda () (json-schema-edit-toggle-required '("properties" "note"))))
    ("change-type" (t) ,(lambda () (json-schema-edit-change-type '("properties" "id") "integer")))
    ("range-string" nil ,(metaschema-generate--range '("properties" "note") "1..200"))
    ("range-number" nil ,(metaschema-generate--range '("properties" "count") "(0..100]"))
    ("rename-property" nil ,(lambda () (json-schema-edit-rename-property '("properties" "id") "orderId")))
    ("rename-definition" nil ,(lambda () (json-schema-edit-rename-definition (list defs "Customer") "Buyer")))))

(defun metaschema-generate (out)
  "Write every transformed base schema into the directory OUT."
  (make-directory out t)
  (dolist (base (directory-files (expand-file-name "test/metaschema") t "\\.json\\'"))
    (let ((draft (file-name-base base)))
      (dolist (tr (with-temp-buffer
                    (insert-file-contents base)
                    (treesit-parser-create 'json)
                    (metaschema-generate--transformations (json-schema-edit-model-defs-keyword))))
        (pcase-let ((`(,name ,answers ,fn) tr))
          (with-temp-buffer
            (insert-file-contents base)
            (treesit-parser-create 'json)
            (setq metaschema-generate--answers (copy-sequence answers))
            (cl-letf (((symbol-function 'read-string) #'metaschema-generate--answer)
                      ((symbol-function 'completing-read) #'metaschema-generate--answer)
                      ((symbol-function 'y-or-n-p) #'metaschema-generate--answer))
              (funcall fn))
            ;; Catch broken JSON here already.
            (goto-char (point-min))
            (json-parse-buffer)
            (write-region (point-min) (point-max)
                          (expand-file-name (format "%s--%s.json" draft name) out)
                          nil 'silent)))))))

(metaschema-generate (or (car command-line-args-left) "metaschema-out"))
(setq command-line-args-left nil)

;;; metaschema-generate.el ends here
