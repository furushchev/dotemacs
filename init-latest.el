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

;; color theme
(load-theme 'tango-dark t)

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

(use-package go-mode
  :init
  (add-to-list 'exec-path (expand-file-name (concat (getenv "GOROOT") "bin")))
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  :config
;;  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (setq indent-tabs-mode nil
                                  c-basic-offset 4 tab-width 4))))

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(use-package irony
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
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
  :config
  (popwin-mode)
  (setq-default popwin:popup-window-height 20)
  (add-to-list 'popwin:special-display-config
               '("*company-documentation*" :position bottom :noselect :dedicated)))

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
  :diminish company-mode
  :config
  (global-company-mode t)
  (setq-default company-idle-delay 0.05
                company-selection-wrap-around t
                company-require-match 'never
                company-auto-complete nil)
  (add-to-list 'company-backends 'company-yasnippet)
  ;; flatten
  (setq company-backends
        (list (apply #'append (mapcar #'(lambda (x) (if (listp x) x (list x))) company-backends))))
  (bind-key "TAB" 'company-complete-common-or-cycle company-active-map)
  (bind-key "S-TAB" 'company-select-previous company-active-map)
  (bind-key "M-[ z" 'company-select-previous company-active-map)
  (bind-key "M-d" 'company-show-doc-buffer company-active-map)
  )


(use-package company-c-headers
  :requires company
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-go
  :requires (company go-mode)
  :config
  (add-to-list 'company-backends 'company-go))

(use-package company-irony
  :requires (company irony)
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-jedi
  :requires (jedi-core company)
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package company-quickhelp
  :requires company
  :config
  (setq-default company-quickhelp-delay 0.5
                company-quickhelp-use-propertized-text t)
  (company-quickhelp-mode))

(use-package company-statistics
  :requires company
  :config
  (company-statistics-mode))

(use-package slime-company
  :requires (slime company)
  :config
  (add-to-list 'slime-contribs 'slime-company))

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
  :diminish git-gutter+
  :config
  (global-git-gutter+-mode))

(use-package git-gutter-fringe+
  :catch (lambda (c a) t))

(use-package slime
  :commands (slime slime-lisp-mode-hook slime-mode)
  :config
  (require 'slime-autoloads)
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-net-coding-system 'utf-8-unix
        slime-protocol-version 'ignore
        slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (dolist (contrib '(slime-asdf
                     slime-banner
                     slime-cl-indent
                     slime-fancy
                     slime-quicklisp))
    (add-to-list 'slime-contribs contrib))
  (slime-setup))

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
