(use-package ace-jump-mode
  :ensure t
  :bind ("\C-." . ace-jump-mode)
  :config (ace-jump-mode-enable-mark-sync))
(use-package ace-jump-mode-pop-mark
  :bind ("C-. SPC" . ace-jump-mode-pop-mark))
