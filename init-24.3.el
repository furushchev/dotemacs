(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; version lock
(el-get-bundle tarao/el-get-lock
  (el-get-lock)
  (el-get-lock-unlock 'el-get 'seq))

(el-get-bundle elpa:diminish)
(el-get-bundle elpa:bind-key)

;; markup

(el-get-bundle elpa:cmake-mode)
(el-get-bundle dockerfile-mode)
(el-get-bundle elpa:google-c-style
  (add-hook 'c-mode 'google-set-c-style)
  (add-hook 'c++-mode 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))
(el-get-bundle graphviz-dot-mode)
(el-get-bundle elpa:irony
  (eval-after-load 'company-irony
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))
(el-get-bundle elpa:irony-eldoc
  (eval-after-load 'irony
    (add-hook 'irony-mode-hook 'irony-eldoc)))
(el-get-bundle jedi-core)
(el-get-bundle markdown-mode)
(el-get-bundle racer)
(el-get-bundle rust-mode)
(el-get-bundle elpa:web-mode
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))
(el-get-bundle elpa:yaml-mode)

;; completion

(el-get-bundle slime)
;; need settings

(el-get-bundle elpa:company
  (global-company-mode)
  (setq company-idle-delay 0.05
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)))
(el-get-bundle elpa:company-c-headers
  (eval-after-load 'company-c-headers
    (add-to-list 'company-backends 'company-c-headers)))
(el-get-bundle elpa:company-irony
  (eval-after-load 'company-irony
    (add-to-list 'company-backends 'company-irony)))
(el-get-bundle company-jedi
  (eval-after-load 'company-jedi
    (add-to-list 'company-backends 'company-jedi)))
(el-get-bundle elpa:company-quickhelp
  (eval-after-load 'company-quicklisp
    (add-hook 'global-company-mode-hook 'company-quickhelp-mode)))
(el-get-bundle elpa:company-statistics)
(el-get-bundle slime-company)

(el-get-bundle yatemplate
  (yatemplate-fill-alist)
  (auto-insert-mode 1))
(el-get-bundle yasnippet)

;; linter

(el-get-bundle flycheck
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-display-errors-functions #'flycheck-display-error-messages-unless-error-list)
   '(flycheck-display-errors-delay 0.5)))
(el-get-bundle flycheck-pos-tip
  (eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; others
(el-get-bundle elpa:popwin)

(provide 'init-24.3)

