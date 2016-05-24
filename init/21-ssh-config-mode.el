(use-package ssh-config-mode
  :ensure t
  :mode (".ssh/config\\'"
         "sshd?_config\\'")
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))
