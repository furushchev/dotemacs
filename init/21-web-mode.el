(use-package web-mode
  :ensure t
  :mode ("\\.phtml$"
         "\\.tpl\\.php$"
         "\\.jsp$"
         "\\.as[cp]x$"
         "\\.erb$"
         "\\.html?$"
         "\\.jsx$"
         "\\.ejs$")
  :config
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "blue")
  (defun web-mode-hook-func ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset    2)
    (setq web-mode-code-offset 2))
  (add-hook 'web-mode-hook 'web-mode-hook-func))
