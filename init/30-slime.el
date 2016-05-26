(use-package slime
  :commands (slime)
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq inferior-lisp-program "sbcl"))
  (use-package slime-autoloads)
  (setq slime-net-coding-system 'utf-8-unix)
  (if (locate-library "rosemacs")
      (slime-setup '(slime-fancy slime-asdf slime-ros slime-repl slime-banner slime-indentation slime-scratch))
    (slime-setup '(slime-fancy slime-asdf slime-repl slime-banner slime-indentation slime-scratch))))

(use-package ac-slime
  :ensure t
  :commands (slime)
  :init
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  :config
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode)))
