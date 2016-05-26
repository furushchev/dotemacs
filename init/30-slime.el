(use-package slime
  :commands (slime)
  :ensure t
  :ensure slime-company
  :config
  (use-package slime-autoloads)

  ;; for slime-repl
  (unbind-key (kbd "DEL") slime-repl-mode-map)

  ;; for slime-company
  (setq slime-company-after-completion 'slime-company-just-one-space)
  (setq slime-company-complete-in-comments-and-strings t)
  (setq slime-company-major-modes '(lisp-mode slime-repl-mode scheme-mode))
  (setq slime-company-completion 'fuzzy)

  (if (eq system-type 'darwin)
      (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq inferior-lisp-program "sbcl"))
  (setq slime-net-coding-system 'utf-8-unix)
  (if (locate-library "rosemacs")
      (slime-setup '(slime-fancy slime-asdf slime-ros slime-company slime-repl slime-banner slime-indentation slime-scratch))
    (slime-setup '(slime-fancy slime-asdf slime-company slime-repl slime-banner slime-indentation slime-scratch))))
