(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
      '(flycheck-display-errors-delay 0.5))))

(use-package flycheck-pos-tip
  :ensure flycheck
  :defer t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))
