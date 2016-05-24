(use-package jedi
  :ensure t
  :mode "\\.py$"
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq-default jedi:complete-on-dot t))
