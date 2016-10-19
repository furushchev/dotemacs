(use-package ssh-config-mode
  :mode (".ssh/config\\'"
         "sshd?_config\\'")
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))
