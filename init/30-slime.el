(use-package slime
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq inferior-lisp-program "sbcl"))
  (use-package slime-autoloads)
  (setq slime-net-coding-system 'utf-8-unix))
