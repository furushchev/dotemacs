(use-package bash-completion
  :ensure t
  :commands (shell)
  :config
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))
