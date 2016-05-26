(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push "*slime-apropos*" popwin:special-display-config)
  (push "*slime-macroexpansion*" popwin:special-display-config)
  (push "*slime-description*" popwin:special-display-config)
  (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
  (push "*slime-xref*" popwin:special-display-config)
  (push '(sldb-mode :stick t) popwin:special-display-config)
  (push 'slime-repl-mode popwin:special-display-config)
  (push 'slime-connection-list-mode popwin:special-display-config))
