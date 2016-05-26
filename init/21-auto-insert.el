(use-package autoinsert
  :config
  (auto-insert-mode t)
  (setq auto-insert-directory (expand-file-name "~/.emacs.d/template"))
  (setq auto-insert-alist
        (nconc '(
                 ("\\.py$"  . ["template.py"   make-template])
                 ("\\.h$"   . ["template.h"    make-template])
                 ("\\.cpp$" . ["template.cpp"  make-template])
                 ("\\.l$"   . ["template.l"    make-template])
                 ) auto-insert-alist))
  (defvar template-replace-alist
    '(("%file%" . (lambda ()
                    (file-name-nondirectory (buffer-file-name))))
      ("%file-without-ext%" . (lambda ()
                                (file-name-sans-extension
                                 (file-name-nondirectory (buffer-file-name)))))
      ("%ros-node-name%" . (lambda ()
                             (replace-regexp-in-string
                              "-" "_"
                              (file-name-sans-extension
                               (file-name-nondirectory (buffer-file-name))))))
      ("%include-guard%" . (lambda ()
                             (format "%s_H__" (upcase
                                               (file-name-sans-extension
                                                (file-name-nondirectory (buffer-file-name)))))))
      ("%name%" . (lambda () (identity user-full-name)))
      ("%email%" . (lambda () (identity user-mail-address)))
      ))

  (defun make-template ()
    (time-stamp)
    (mapc #'(lambda (c)
              (progn
                (goto-char (point-min))
                (replace-string (car c) (funcall (cdr c)) nil)))
          template-replace-alist)
    (goto-char (point-max))
    (message "done."))

  (add-hook 'find-file-not-found-hooks 'auto-insert))
