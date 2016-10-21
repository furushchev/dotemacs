(use-package diff-hl
  :ensure t
  :defer t
  :init
  (global-diff-hl-mode)
  :config
  (set-face-attribute 'diff-hl-insert nil
                      :foreground "green" :weight 'bold)
  (set-face-attribute 'diff-hl-delete nil
                      :foreground "red" :weight 'bold)
  (set-face-attribute 'diff-hl-change nil
                      :foreground "magenta" :weight 'bold)

  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))
