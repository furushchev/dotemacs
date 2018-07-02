(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle company-quickhelp
  (eval-after-load 'pos-tip
    (eval-after-load 'company-mode
      (add-hook 'global-company-mode-hook 'company-quickhelp-mode))))

(el-get-bundle jedi-core
  (load "~/.emacs.d/subr-x.el")
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup))

(defvar company-backends nil)
(el-get-bundle company-jedi
  (add-to-list 'company-backends 'company-jedi))

(el-get-bundle markdown-mode)
(el-get-bundle pos-tip)
(el-get-bundle slime-company)

;; use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/elpa")
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package diminish :ensure t)
(require 'bind-key)

(use-package abbrev
  :diminish abbrev-mode
  :defer t)

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package company
  ;; :diminish company-mode
  :ensure t
  :config
  (global-company-mode 1)
  (bind-key [remap completion-at-point] #'company-complete comapny-mode-map)
  (setq company-idle-delay 0.05
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)))


(use-package company-c-headers
  :requires company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-irony
  :ensure t
  :requires company
  :config
  (add-to-list 'company-backends 'company-irony))

(unless (locate-library "company-quickhelp")
  (use-package company-quickhelp
    :ensure t
    :hook (global-company-mode . company-quickhelp-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
   '(flycheck-display-errors-delay 0.5)))

(use-package flycheck-pos-tip
  :requires (flycheck)
  :ensure t
  :config
  (flycheck-pos-tip-mode))

(use-package google-c-style
  :ensure t
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style))
  :config
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :requires irony-mode
  :hook (irony-mode . irony-eldoc))

(unless (locate-library "jedi-core")
  (use-package jedi-core
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (load "~/.emacs.d/subr-x.el")
    (setq-default jedi:complete-on-dot t)
    (setq-default jedi:use-shortcuts t)))

(unless (locate-library "company-jedi")
  (use-package company-jedi
    :ensure t
    :requires (jedi-core company)
    :config
    (add-to-list 'company-backends 'company-jedi)))

(unless (locate-library "markdown-mode")
  (use-package markdown-mode
    :ensure t
    :mode (("\\.markdown\\'" . gfm-mode)
           ("\\.md\\'" . gfm-mode))))

(use-package popwin
  :ensure t
  :commands popwin-mode)

;; save last cursor place
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

;; slime
(unless (locate-library "slime-company")
  (use-package slime-company
    :ensure t
    :defer t
    :commands slime-company))

(add-to-list 'load-path "/home/furushchev/euslime")
(require 'euslime nil t)
(use-package slime
  ;;  :after slime-company
  :ensure t
  :commands (slime slime-lisp-mode-hook slime-mode)
  :config
  (require 'slime-autoloads)
  (slime-setup
   '(slime-fancy slime-asdf slime-quicklisp slime-cl-indent slime-company))
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-net-coding-system 'utf-8-unix
        slime-protocol-version 'ignore
        slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
)

;; use directory name instead of <num>
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; writable dired
(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions t)
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode))

(use-package web-mode
  :ensure t
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
  :ensure t
  :mode "\\.ya?ml$")

(use-package yatemplate
  :ensure t
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1))

(use-package yasnippet
  :after company
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/el-get/yasnippet/snippets"))
  (define-key yas-keymap (kbd "<tab>") nil)
  (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

(provide 'init-24.3)

