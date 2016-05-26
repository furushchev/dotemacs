(use-package with-editor
  :ensure t)
(use-package git-gutter+
  :load-path "site-lisp/"
  :diminish git-gutter+-mode
  :config (global-git-gutter+-mode))
