;;; run-checkdoc.el --- Run checkdoc and fail on warnings  -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage: emacs -Q --batch -L . -l test/run-checkdoc.el FILE...

;;; Code:

(require 'checkdoc)

(defvar run-checkdoc--warnings 0
  "Number of warnings reported by checkdoc.")

(advice-add 'display-warning :before
            (lambda (&rest _) (setq run-checkdoc--warnings (1+ run-checkdoc--warnings))))

(dolist (file command-line-args-left)
  (checkdoc-file file))
(setq command-line-args-left nil)

(kill-emacs (if (zerop run-checkdoc--warnings) 0 1))

;;; run-checkdoc.el ends here
