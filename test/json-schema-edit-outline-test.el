;;; json-schema-edit-outline-test.el --- Tests for the outline  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `json-schema-edit-outline'.

;;; Code:

(require 'ert)
(require 'json-schema-edit-mode)

(when-let* ((dir (getenv "TREESIT_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(defconst json-schema-edit-outline-test--fixtures
  (expand-file-name "fixtures/" (file-name-directory
                                 (or load-file-name buffer-file-name))))

(defmacro json-schema-edit-outline-test--with-outline (name &rest body)
  "Copy fixture NAME to a temporary directory, open its outline and run BODY.
BODY runs in the outline buffer; `source' is bound to the schema buffer."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "jse-outline" t))
          (file (expand-file-name ,name dir)))
     (copy-file (expand-file-name ,name json-schema-edit-outline-test--fixtures) file)
     (let ((source (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer source
             (treesit-parser-create 'json)
             (json-schema-edit-mode 1)
             (with-current-buffer (json-schema-edit-outline-buffer)
               (json-schema-edit-outline-refresh)
               (goto-char (point-min))
               ,@body))
         (with-current-buffer source
           (when (buffer-live-p json-schema-edit-outline--buffer)
             (kill-buffer json-schema-edit-outline--buffer))
           (set-buffer-modified-p nil))
         (kill-buffer source)
         (delete-directory dir t)))))

(defun json-schema-edit-outline-test--lines ()
  "Return the outline as \"PREFIX LABEL | TYPE | NOTE\" strings."
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((e (get-text-property (point) 'json-schema-edit-outline-entry)))
          (let* ((text (buffer-substring-no-properties (pos-bol) (pos-eol)))
                 (type (json-schema-edit-outline-entry-type e))
                 (note (json-schema-edit-outline-entry-note e))
                 (label-end (or (string-search (concat " " (or type note "\0")) text)
                                (length text))))
            (push (string-trim-right
                   (concat (string-trim-right (substring text 0 label-end))
                           (when (or type note) (concat " | " (or type "")))
                           (when note (concat " | " note))))
                  lines)))
        (forward-line 1)))
    (nreverse lines)))

(defun json-schema-edit-outline-test--goto (label)
  "Move to the first outline line whose label is LABEL."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (let ((e (get-text-property (point) 'json-schema-edit-outline-entry)))
        (if (and e (equal (json-schema-edit-outline-entry-label e) label))
            (setq found t)
          (forward-line 1))))
    (should found)))

(defun json-schema-edit-outline-test--source-json (source)
  "Parse SOURCE into alists."
  (with-current-buffer source
    (save-excursion
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

;;;; Display

(ert-deftest json-schema-edit-outline-test-basic ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (should (equal (json-schema-edit-outline-test--lines)
                   '("User | object"
                     "├─ id * | string  (uuid)"
                     "├─ name * | string  1..100"
                     "├─ address | → Address"
                     "├─ home | → Address"
                     "├─ tags | → #tag[]"
                     "├─ audit | → Timestamps"
                     "├─ contact | oneOf"
                     "│  ├─ ◇ email | object"
                     "│  │  └─ address | string"
                     "│  └─ ◇ Phone | → Phone"
                     "├─ broken | → Missing"
                     "└─ slash | → a/b"
                     "$defs"
                     "├─ Address | object"
                     "│  ├─ zip * | string  ^\\d{3}-\\d{4}$"
                     "│  └─ city | string"
                     "├─ Phone | string"
                     "├─ Tag | string"
                     "└─ a/b | string?")))))

(ert-deftest json-schema-edit-outline-test-composite ()
  (json-schema-edit-outline-test--with-outline "composite.schema.json"
    (should (equal (seq-take (json-schema-edit-outline-test--lines) 22)
                   '("User | object"
                     "├─ id * | string  (uuid)"
                     "├─ nickname | string?"
                     "├─ amount | number | string | anyOf"
                     "├─ age | integer  (0..150]"
                     "├─ status | \"active\" | \"gone\" | oneOf"
                     "│  ├─ \"active\" |  | Active"
                     "│  └─ \"gone\""
                     "├─ tags | string[]  unique"
                     "├─ contact * | oneOf by kind"
                     "│  ├─ ◇ email | object"
                     "│  │  ├─ kind * | = \"email\""
                     "│  │  └─ address | string  (email)"
                     "│  └─ ◇ phone | object"
                     "│     ├─ kind | = \"phone\""
                     "│     └─ number | string  ^\\+?\\d+$"
                     "├─ misc | oneOf"
                     "│  ├─ ◇ #1 | object"
                     "│  │  └─ a"
                     "│  └─ ◇ #2 | array"
                     "└─ ⊂ extends Timestamps |  | allOf"
                     "   ├─ createdAt * | string  (date-time) | ↖ Timestamps")))
    (should (member "├─ Pet | oneOf by petType" (json-schema-edit-outline-test--lines)))
    (should (member "│  ├─ ◇ cat | object" (json-schema-edit-outline-test--lines)))))

(ert-deftest json-schema-edit-outline-test-merge-allof ()
  (json-schema-edit-outline-test--with-outline "composite.schema.json"
    (json-schema-edit-outline-toggle-merge)
    (should (equal (seq-take (json-schema-edit-outline-test--lines) 4)
                   '("User | object"
                     "├─ createdAt * | string  (date-time) | ↖ Timestamps"
                     "├─ updatedAt | string  (date-time) | ↖ Timestamps"
                     "├─ id * | string  (uuid)")))
    ;; Inherited lines point into the base definition.
    (json-schema-edit-outline-test--goto "createdAt")
    (should (equal (json-schema-edit-outline-entry-pointer
                    (get-text-property (point) 'json-schema-edit-outline-entry))
                   '("$defs" "Timestamps" "properties" "createdAt")))))

(ert-deftest json-schema-edit-outline-test-cycle ()
  (with-temp-buffer
    (insert "{\"$defs\": {\"A\": {\"allOf\": [{\"$ref\": \"#/$defs/A\"}]}}}")
    (treesit-parser-create 'json)
    (should (json-schema-edit-outline-entries))))

;;;; Folding and refresh

(ert-deftest json-schema-edit-outline-test-headings ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "contact")
    (should (= (funcall outline-level) 2))
    (outline-hide-subtree)
    (should (outline-invisible-p (pos-eol)))
    (outline-next-visible-heading 1)
    (should (equal (json-schema-edit-outline-entry-label
                    (get-text-property (point) 'json-schema-edit-outline-entry))
                   "broken"))))

(ert-deftest json-schema-edit-outline-test-refresh-keeps-state ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "contact")
    (outline-hide-subtree)
    (json-schema-edit-outline-test--goto "zip")
    (with-current-buffer source
      (goto-char (point-min))
      (search-forward "\"city\"")
      (replace-match "\"town\""))
    (json-schema-edit-outline-refresh)
    (should (equal (json-schema-edit-outline-entry-label
                    (get-text-property (point) 'json-schema-edit-outline-entry))
                   "zip"))
    (json-schema-edit-outline-test--goto "contact")
    (should (outline-invisible-p (pos-eol)))
    (should (member "│  └─ town | string" (json-schema-edit-outline-test--lines)))))

(ert-deftest json-schema-edit-outline-test-auto-refresh ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (with-current-buffer source
      (goto-char (point-min))
      (search-forward "\"city\"")
      (replace-match "\"town\"")
      (should (timerp json-schema-edit-outline--timer))
      (timer-event-handler json-schema-edit-outline--timer))
    (should (member "│  └─ town | string" (json-schema-edit-outline-test--lines)))))

;;;; Commands

(ert-deftest json-schema-edit-outline-test-visit ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "zip")
    (save-window-excursion
      (json-schema-edit-outline-visit)
      (should (eq (current-buffer) source))
      (should (looking-at-p "\"zip\"")))))

(ert-deftest json-schema-edit-outline-test-toggle-required ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "city")
    (json-schema-edit-outline-toggle-required)
    (should (member "│  └─ city * | string" (json-schema-edit-outline-test--lines)))
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should (equal .$defs.Address.required '("zip" "city"))))
    ;; Point stays on the line, so toggling again undoes it.
    (json-schema-edit-outline-toggle-required)
    (json-schema-edit-outline-test--goto "zip")
    (json-schema-edit-outline-toggle-required)
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should-not (assq 'required .$defs.Address)))
    (json-schema-edit-outline-test--goto "$defs")
    (should-error (json-schema-edit-outline-toggle-required) :type 'user-error)))

(ert-deftest json-schema-edit-outline-test-change-type ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "name")
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "integer"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (json-schema-edit-outline-change-type))
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should (equal .properties.name '((type . "integer")))))
    (should (member "├─ name * | integer" (json-schema-edit-outline-test--lines)))
    ;; To a $ref, keeping the keywords when declined.
    (json-schema-edit-outline-test--goto "id")
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "#/$defs/Tag")))
      (json-schema-edit-outline-change-type))
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should (equal .properties.id '((format . "uuid") ($ref . "#/$defs/Tag")))))))

(ert-deftest json-schema-edit-outline-test-rename ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "zip")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "postcode")))
      (json-schema-edit-outline-rename))
    (should (equal (json-schema-edit-outline-entry-label
                    (get-text-property (point) 'json-schema-edit-outline-entry))
                   "postcode"))
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should (equal .$defs.Address.required '("postcode"))))
    (json-schema-edit-outline-test--goto "Phone")
    (forward-line 0)
    (while (not (eq (json-schema-edit-outline-entry-kind
                     (get-text-property (point) 'json-schema-edit-outline-entry))
                    'definition))
      (forward-line 1))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Tel")))
      (json-schema-edit-outline-rename))
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should .$defs.Tel)
      (should (equal (caddr (assq 'oneOf .properties.contact)) '(($ref . "#/$defs/Tel")))))))

(ert-deftest json-schema-edit-outline-test-add-property ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (json-schema-edit-outline-test--goto "city")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "country"))
              ((symbol-function 'completing-read) (lambda (&rest _) "string"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (json-schema-edit-outline-add-property))
    (should (equal (json-schema-edit-outline-entry-label
                    (get-text-property (point) 'json-schema-edit-outline-entry))
                   "country"))
    (should (member "│  └─ country * | string" (json-schema-edit-outline-test--lines)))
    ;; On the root, the property goes into it.
    (goto-char (point-min))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "age"))
              ((symbol-function 'completing-read) (lambda (&rest _) "integer"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (json-schema-edit-outline-add-property))
    (let-alist (json-schema-edit-outline-test--source-json source)
      (should (equal .properties.age '((type . "integer"))))
      (should (equal .$defs.Address.required '("zip" "country"))))))

(ert-deftest json-schema-edit-outline-test-ignore-structure-edit ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (should (eq (key-binding [remap outline-move-subtree-up])
                #'json-schema-edit-outline-ignore))
    (should (eq (key-binding (kbd "r")) #'json-schema-edit-outline-toggle-required))))

;;;; Opening and imenu

(ert-deftest json-schema-edit-outline-test-open-at-point ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (with-current-buffer source
      (goto-char (point-min))
      (search-forward "\"pattern\"")
      (save-window-excursion
        (json-schema-edit-outline)
        (with-current-buffer (buffer-local-value 'json-schema-edit-outline--buffer source)
          (should (equal (json-schema-edit-outline-entry-pointer
                          (get-text-property (point) 'json-schema-edit-outline-entry))
                         '("$defs" "Address" "properties" "zip"))))))))

(ert-deftest json-schema-edit-outline-test-imenu ()
  (json-schema-edit-outline-test--with-outline "user.schema.json"
    (with-current-buffer source
      (let* ((index (funcall imenu-create-index-function))
             (defs (assoc "$defs" index))
             (address (assoc "Address" (cddr defs))))
        (should (equal (mapcar #'car index) '("User" "$defs")))
        (should (equal (mapcar #'car (cdr address)) '("Address" "zip" "city")))
        (goto-char (cdr (assoc "zip" (cdr address))))
        (should (looking-at-p "\"zip\""))))))

(provide 'json-schema-edit-outline-test)
;;; json-schema-edit-outline-test.el ends here
