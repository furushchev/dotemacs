;; save place
(require 'saveplace)
(with-eval-after-load 'saveplace
  (save-place-mode t))

(use-package auto-compile
  :no-require t
  :defer t
  :ensure t
  :diminish "C"
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(use-package bind-key
  :no-require t)

(use-package company
  :no-require t
  :diminish "company"
  :config
  (global-company-mode t)
  (bind-keys :map company-active-map
             ("C-i" . 'company-complete-common-or-cycle)
             ("TAB" . 'company-complete-common-or-cycle)
             ("M-." . 'company-show-doc-buffer))
  (bind-keys :map company-search-map
             ("S-TAB" . 'company-select-previous)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

(use-package company-lsp
  :no-require t
  :commands company-lsp)

;; (bind-key "S-TAB" 'company-select-previous company-active-map)
;; (bind-key "M-[ z" 'company-select-previous company-active-map)
;; (bind-key "M-d" 'company-show-doc-buffer company-active-map))


(use-package cmake-mode
  :no-require t
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package cuda-mode
  :no-require t
  :mode ("\\.cu\\'" "\\.cuh\\'"))

(use-package dockerfile-mode
  :no-require t
  :mode "Dockerfile\\'")

(use-package flycheck
  :no-require t
  :defer t
  :ensure t
  :diminish "FlyC"
  :config
  (global-flycheck-mode t)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17"))))

(use-package git-gutter+
  :no-require t
  :diminish git-gutter+
  :config
  (global-git-gutter+-mode))

(use-package lsp-mode
  :no-require t
  :commands (lsp lsp-deferred)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-clangd-executable "clangd-6.0")
  (setq lsp-prefer-flymake nil))

;; (use-package lsp-ui
;;   :no-require t
;;   :commands lsp-ui-mode)

(use-package magit
  :no-require t
  :bind ("C-x m" . magit-status))

(use-package markdown-mode
  :no-require t
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)))

(use-package rosemacs
  :straight nil
  :load-path (lambda () (format "/opt/ros/%s/share/emacs/site-lisp"
                                (or (getenv "ROS_DISTRO") "melodic")))
  :config
  (setq ros-topic-update-interval 0
        ros-node-update-interval 0)
  (invoke-rosemacs)
  (bind-key "C-x C-r" ros-keymap))

(use-package web-mode
  :no-require t
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
  :no-require t
  :mode "\\.ya?ml$")

(use-package yatemplate
  :no-require t
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1))

(use-package yasnippet
  :no-require t
  :diminish yas-minor-mode
  :config
  (bind-key "TAB" nil yas-keymap)
  (yas-global-mode t))

;;
