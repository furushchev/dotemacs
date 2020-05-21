(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :config
  (setq system-packages-usesudo t))

(use-package diminish)

(use-package arduino-mode
  :no-require t
  :commands arduino-mode)

(use-package bind-key
  :no-require t)

(use-package company
  :no-require t
  :diminish
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

(use-package cmake-mode
  :no-require t
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))

(use-package cuda-mode
  :no-require t
  :mode ("\\.cu\\'" "\\.cuh\\'"))

(use-package dockerfile-mode
  :no-require t
  :mode "Dockerfile\\'")

(use-package eldoc
  :straight nil
  :config
  :diminish)

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
  :diminish
  :config
  (global-git-gutter+-mode))
(defun git-gutter+-remote-default-directory (dir file)
  (let* ((vec (tramp-dissect-file-name file))
         (method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (domain (tramp-file-name-domain vec))
         (host (tramp-file-name-host vec))
         (port (tramp-file-name-port vec)))
    (tramp-make-tramp-file-name method user domain host port dir)))
(defun git-gutter+-remote-file-path (dir file)
  (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
    (replace-regexp-in-string (concat "\\`" dir) "" file)))

(use-package lsp-mode
  :no-require t
  :ensure-system-package
  (("/usr/local/bin/pyls" . "pip install python-language-server[all]")
   ("/usr/bin/clangd-6.0" . clang-tools-6.0))
  :commands (lsp lsp-deferred)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; (setq lsp-log-io nil
  ;;       lsp-enable-folding nil
  ;;       lsp-diagnostic-package :none
  ;;       lsp-enable-snippet nil
  ;;       lsp-enable-symbol-highlighting nil
  ;;       lsp-enable-links nil
  ;;       lsp-prefer-flymake nil
  ;;       lsp-restart 'auto-restart
  ;;       )
  (setq lsp-clients-clangd-executable "clangd-6.0")
  (setq lsp-pyls-plugins-pylint-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-pyflakes-enabled nil
        lsp-pyls-plugins-autopep8-enabled nil
        lsp-pyls-plugins-yapf-enabled t))

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

(use-package saveplace
  :straight nil
  :config
  (save-place-mode t))

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

(use-package which-key
  :config
  (which-key-mode))

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
