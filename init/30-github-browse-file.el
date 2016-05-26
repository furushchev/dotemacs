(use-package github-browse-file
  :ensure t
  :commands (github-browse-file)
  :config (setq github-browse-file-show-line-at-point t)
  :bind ("C-c gh" . github-browse-file))
