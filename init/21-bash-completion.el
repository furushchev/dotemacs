(use-package bash-completion
  :defer t
  :ensure t
  :commands shell
  :config
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))
