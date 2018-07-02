;; setup straight.el

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq package-enable-at-startup nil
      straight-use-package-by-default t)

(use-package diminish)
(use-package bind-key)

;; language

(use-package cmake-mode
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package google-c-style
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style))
  :config
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(use-package irony
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :requires irony
  :hook ((irony-mode . irony-eldoc)))

(use-package jedi-core
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  ;; (load "~/.emacs.d/subr-x.el")
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)))

(use-package popwin
  :commands popwin-mode)

(use-package web-mode
  :mode (("\\.phtml$" . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.html?$" . web-mode)
         ("\\.jsx?$" . web-mode)
         ("\\.ejs$" . web-mode))
  :config
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(use-package yaml-mode
  :mode "\\.ya?ml$")


;; company

(use-package company
  ;; :diminish company-mode
  :after (company-c-headers company-irony company-jedi company-quickhelp yasnippet)
  :init
  (setq company-backends '())
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.05
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-auto-complete t)
  (setq company-backends
        '((company-jedi
           company-capf
           company-c-headers
           company-irony
           company-clang
           company-gtags
           company-etags
           company-keywords
           company-cmake
           company-nxml
           company-css
           company-yasnippet
           company-semantic
           company-files
           company-dabbrev-code)
          company-dabbrev))
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend))
  (bind-key "TAB" 'company-complete-common-or-cycle company-active-map)
  (bind-key "S-TAB" 'company-select-previous company-active-map)
  (bind-key "M-[ z" 'company-select-previous company-active-map)
  )


(use-package company-c-headers
  ;; :requires company
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-irony
  ;; :requires company
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-jedi
  ;; :requires (jedi-core company)
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package company-quickhelp
  ;; :requires company
  ;; :commands global-company-mode
  :config
  (company-quickhelp-mode t))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package slime-company
  :requires (slime company)
  :commands slime-company)

;; others

(use-package flycheck
  :diminish flycheck-mode
  :disabled
  :config
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-display-errors-delay 1.0)))

(use-package flycheck-pos-tip
  :after flycheck
  :disabled
  :config
  (custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package git-gutter+
  :config
  (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :catch (lambda (c a) t))

(use-package slime
  :commands (slime slime-lisp-mode-hook slime-mode)
  :config
  (require 'slime-autoloads)
  (slime-setup
   '(slime-fancy slime-asdf slime-quicklisp slime-cl-indent slime-company))
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-net-coding-system 'utf-8-unix
        slime-protocol-version 'ignore
        slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

(use-package yatemplate
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/el-get/yasnippet/snippets"))
  (bind-key "TAB" nil yas-keymap)
  (yas-global-mode t))
