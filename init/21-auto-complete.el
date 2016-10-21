(use-package auto-complete
  :disabled t
  :ensure t
  :ensure auto-complete-config
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
  (global-auto-complete-mode t)
  (setq ac-use-menu-map t)
  (setq ac-auto-start 2)
  (setq ac-delay 0.05)
  (setq ac-use-fuzzy t)
  (setq ac-use-comphist t)
  (setq ac-auto-show-menu 0.05)
  (setq ac-quick-help-delay 0.5)
  (setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
  ;; emacs-lisp
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-symbols t)))
  (setq popup-use-optimized-column-computation nil)
  (dolist (mode '(log-edit-mode org-mode text-mode
                                yaml-mode haskell-mode html-mode
                                nxml-mode sh-mode lisp-mode markdown-mode
                                js2-mode css-mode))
    (add-to-list 'ac-modes mode))
  (ac-flyspell-workaround)
  ;; avoid japanese
  (defadvice ac-word-candidates (after remove-word-contain-japanese activate)
    (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
      (setq ad-return-value
            (remove-if contain-japanese ad-return-value))))
  :bind
  (:map ac-completing-map
        ("C-n" . ac-next)
        ("C-p" . ac-previous)))
