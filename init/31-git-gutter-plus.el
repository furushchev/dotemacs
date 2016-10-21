(use-package git-gutter+
  :load-path "site-lisp/"
  :ensure t
  :defer t
  :diminish git-gutter+-mode
  :config (global-git-gutter+-mode))
