;;; lint-elisp.el --- Structural check over this config's own elisp -*- lexical-binding: t; -*-
;;
;; Run as:  emacs --batch -l .github/ci/lint-elisp.el
;;
;; `check-parens' rather than a full byte-compile: byte-compiling init.el in
;; isolation reports a wall of "not known to be defined" for every `leaf'
;; keyword and every autoload, which drowns out anything real.  Unbalanced
;; parens and unterminated strings are the failure mode that actually bricks a
;; config, and they are exactly what this catches -- cheaply, before the
;; multi-minute package bootstrap runs.
;;
;; Only our own sources are checked.  Vendored and installed trees
;; (elpa/, el-get/, the systemrdl-mode submodule) are upstream's problem.

(require 'subr-x)

(defconst lint/skip-dirs
  '("." ".." ".git" "elpa" "el-get" "straight" "backup" "eln-cache" "snippets"
    "systemrdl-mode")
  "Directory names never descended into.
Dotted directories are not skipped wholesale -- .github holds these very
scripts, and they should hold themselves to the same check.")

(defun lint/collect (dir)
  "Return our own .el files under DIR, recursively."
  (let (files)
    (dolist (entry (directory-files dir t nil t))
      (let ((base (file-name-nondirectory entry)))
        (cond
         ((file-directory-p entry)
          (unless (member base lint/skip-dirs)
            (setq files (nconc files (lint/collect entry)))))
         ((string-suffix-p ".el" base)
          (push entry files)))))
    files))

(let* ((root (expand-file-name user-emacs-directory))
       (files (sort (lint/collect root) #'string<))
       (failures 0))
  (when (null files)
    (princ (format "FAIL: found no elisp to lint under %s\n" root))
    (kill-emacs 1))
  (dolist (f files)
    (let ((rel (file-relative-name f root)))
      (with-temp-buffer
        (insert-file-contents f)
        (emacs-lisp-mode)
        (condition-case err
            (progn
              (check-parens)
              (princ (format "  ok    %s\n" rel)))
          (error
           (setq failures (1+ failures))
           (princ (format "  FAIL  %s: %s\n" rel (error-message-string err))))))))
  (princ (format "\n%d file(s) checked, %d failure(s)\n" (length files) failures))
  (kill-emacs (if (> failures 0) 1 0)))

;;; lint-elisp.el ends here
