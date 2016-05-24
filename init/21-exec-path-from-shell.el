(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :defer t
  :ensure t
  :commands shell
  :config (exec-path-from-shell-initialize))
