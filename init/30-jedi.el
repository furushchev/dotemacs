(use-package company-jedi
  :ensure jedi-core
  :ensure company-jedi
  :ensure python-environment
  :mode "\\.py$"
  :config
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t)

  (define-key jedi-mode-map (kbd "M-?") 'jedi:show-doc)

  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi))
