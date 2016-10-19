(use-package bash-completion
  :commands (shell)
  :config
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))
