(use-package slime
  :ensure t
  :ensure slime-company
  :commands (slime)
  :config
  (use-package slime-autoloads)
  ;; for slime-company
  (setq slime-company-after-completion 'slime-company-just-one-space)
  (setq slime-company-complete-in-comments-and-strings t)
  (setq slime-company-major-modes '(lisp-mode slime-repl-mode scheme-mode))
  (setq slime-company-completion 'fuzzy)

  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-fancy slime-asdf slime-company slime-repl slime-banner slime-indentation slime-scratch)))
