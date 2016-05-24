(add-hook 'after-init-hook #'global-flycheck-mode)
#|
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
|#
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)))
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'scala-mode-hook 'flycheck-mode)
(add-hook 'yaml-mode-hook 'flycheck-mode)
(add-hook 'coffee-mode-hook 'flycheck-mode)
