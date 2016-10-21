(use-package irony
  :ensure t
  :ensure company
  :ensure company-irony
  :config
  (setq irony-lang-compile-option-alist
        (quote ((c++-mode . "c++ -std=c++11 -lstdc++")
                (c-mode . "c")
                (objc-mode . "objective-c"))))
  (if (version< emacs-version "24.4")
    (progn
      (defadvice irony--lang-compile-option (around ad-irony--lang-compile-option activate)
        (defvar irony-lang-compile-option-alist)
        (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
          (when it (append '("-x") (split-string it "\s"))))))
    (progn ;; else
      (defun ad-irony--lang-compile-option ()
        (defvar irony-lang-compile-option-alist)
        (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
          (when it (append '("-x") (split-string it "\s")))))
      (advice-add 'irony--lang-compile-option :override #'ad-irony--lang-compile-option)))
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
