(use-package git-gutter+
  :disabled t
  :load-path "site-lisp/"
  :load-path "site-lisp/git-modes/"
  :diminish git-gutter+-mode
  :config (global-git-gutter+-mode))
