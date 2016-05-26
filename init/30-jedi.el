(use-package jedi
  :ensure jedi-core
  :ensure company-jedi
  :mode "\\.py$"
  :config
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi))
