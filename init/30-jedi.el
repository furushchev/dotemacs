(use-package company-jedi
  :ensure t
  :ensure jedi-core
  :ensure python-environment
  :mode "\\.py$"
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  :config
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t)

  (define-key jedi-mode-map (kbd "M-?") 'jedi:show-doc))

