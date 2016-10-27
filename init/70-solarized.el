(use-package color-theme-solarized
  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "~/.emacs.d/site-lisp/emacs-color-theme-solarized"))
  (load-theme 'solarized t)
  (set-frame-parameter nil 'background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized)
  )
