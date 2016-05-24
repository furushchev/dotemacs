(require 'auto-complete)
(require 'auto-complete-config)
(defvar ac-directory-directories nil)
(add-to-list 'ac-directory-directories (expand-file-name "~/.emacs.d/ac-dict"))
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-use-menu-map t)
(setq ac-auto-start 2)
(setq ac-delay 0.05)
(setq ac-use-fuzzy t)
(setq ac-use-comphist t)
(setq ac-auto-show-menu 0.05)
(setq ac-quick-help-delay 0.5)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))

;; emacs-lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-symbols t)))

(setq popup-use-optimized-column-computation nil)

(add-to-list 'ac-modes 'markdown-mode)

(ac-flyspell-workaround)

;; avoid japanese
(defadvice ac-word-candidates (after remove-word-contain-japanese activate)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value
          (remove-if contain-japanese ad-return-value))))
