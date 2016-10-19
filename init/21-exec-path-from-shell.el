(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :commands (shell)
  :config (exec-path-from-shell-initialize))
