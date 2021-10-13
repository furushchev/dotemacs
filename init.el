;; .emacs.d/init.el
;;
;; Author: Yuki Furuta <me@furushchev.ru>
;;

;; Setup leaf.el
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (when (< emacs-major-version 26)
    (setq package-check-signature nil)
    ;; dummy function for blackout error
    (defun blackout (&rest args) t))
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    :config
    (leaf-keywords-init)
    (leaf blackout :emacs>= 26 :ensure t)
    (leaf el-get :ensure t)))

(leaf leaf
  :doc "Install leaf-convert after leaf is enabled"
  :config
  (leaf leaf-convert :emacs>= 26 :ensure t))

;; Setup variables
(leaf custom-keybinding
  :doc "Custom variables for keybinding"
  :tag "builtin"
  :bind (("C-c c" . comment-region)
         ("C-c u" . uncomment-region)
         ("M-g" . goto-line)
         ("M-ESC ESC" . keyboard-quit))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf custom-user
  :doc "Custom variables for user"
  :tag "builtin"
  :custom `((user-full-name . "Yuki Furuta")
            (user-login-name . "furushchev")
            (user-mail-address . "me@furushchev.ru"))
  :config
  (defun user-mail-address () user-mail-address))

(leaf custom-edit
  :doc "Custom variables for editing"
  :tag "builtin"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))
            (debug-on-error . t)
            (display-warning-minimum-level . :error)
            (enable-local-variables . :safe)
            (enable-recursive-minibuffers . t)
            (frame-resize-pixelwise . t)
            (gc-cons-threshold . ,(* 50 gc-cons-threshold))
            (history-delete-duplicates . t)
            (history-length . 1000)
            (indent-tabs-mode . nil)
            (init-file-debug . t)
            (locale-coding-system . 'utf-8)
            (read-process-output-max . ,(* 4 1024 1024))
            (require-final-newline . t)
            (tab-width . 2)
            (truncate-lines . t))
  :config
  (global-font-lock-mode t)
  (prefer-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-buffer-multibyte t)
  (set-clipboard-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (set-file-name-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-language-environment 'Japanese)
  (set-terminal-coding-system 'utf-8-unix))

(leaf custom-appearance
  :doc "Custom variables for appearance"
  :tag "builtin"
  :custom `((bidi-display-reordering . nil)
            (column-number-mode . t)
            (frame-background-mode . 'dark)
            (inhibit-startup-screen . t)
            (inhibit-startup-message . t)
            (initial-scratch-messaage . nil)
            (line-number-mode . t)
            (linum-delay . t)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (scroll-bar-mode . nil)
            (scroll-conservatively . 100)
            (scroll-preserve-screen-position . t)
            (show-paren-delay . 0)
            (show-paren-mode . t)
            (text-quoting-style . 'straight))
  :config
  (defadvice linum-schedule (around my-linum-sched () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current)))

(leaf custom-window-system
  :doc "Custom variables on window system"
  :tag "builtin"
  :when window-system
  :config
  (load-theme 'tango-dark t nil))

(leaf backup
  :doc "Backup files"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (make-backup-files . t)
            (version-control . t)
            (delete-old-versions . t)
            (auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1)
           (auto-revert-verbose . nil)
           (global-auto-revert-non-file-buffers . t))
  :global-minor-mode global-auto-revert-mode)

(leaf make-directories
  :doc "Make directory if not exists"
  :tag "builtin"
  :hook (find-file-not-found-functions . (lambda ()
                                           (let ((dir (file-name-directory (buffer-file-name))))
                                             (make-directory dir t) nil))))

(leaf make-executable
  :doc "Make script file executable if it contains shebang"
  :tag "builtin"
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :tag "builtin"
  :added "2021-01-04"
  :blackout t)

(leaf saveplace
  :doc "automatically save place in files"
  :tag "builtin"
  :added "2021-01-04"
  :global-minor-mode save-place-mode)

(leaf uniquify
  :doc "unique buffer names dependent on file name"
  :tag "builtin" "files"
  :added "2021-01-04"
  :require t
  :config
  (setq-default uniquify-buffer-name-style 'forward)
  (put 'dired-find-alternate-file 'disabled nil))

(leaf cc-mode
  :doc "user customization variables for CC Mode"
  :tag "builtin"
  :added "2021-01-04"
  :defvar (c-basic-offset)
  :mode ((("\\.c$" "\\.h$") . c-mode)
         (("\\.cc$" "\\.hh$" "\\.cxx$" "\\.hxx$" "\\.cpp$" "\\.hpp$") . c++-mode))
  :mode-hook
  (c-mode-common-hook . ((c-set-style "linux")
                         (setq c-basic-offset tab-width)
                         (c-set-offset 'inline-open 0)
                         (c-set-offset 'inline-close 0)
                         (c-set-offset 'member-init-intro 0)
                         (c-set-offset 'innamespace 0)
                         (c-set-offset 'arglist-intro '++))))

(leaf sh-mode
  :doc "Shell mode properties"
  :tag "builtin"
  :mode ("\\.sh$" "\\.bash$" "\\.zsh$")
  :custom `((sh-basic-offset . ,tab-width)
            (sh-indentation . ,tab-width)))

(leaf whitespace
  :doc "minor mode to visualize HARD TAB, ZENKAKU SPACE"
  :tag "builtin"
  :added "2021-01-04"
  :global-minor-mode global-whitespace-mode
  :custom '((whitespace-space-regexp . "\\(\u3000+\\)")
            (whitespace-style . '(face tabs tab-mark spaces space-mark))
            (whitespace-display-mappings . '()))
  :config
  (set-face-background 'whitespace-space "red")
  (set-face-background 'whitespace-tab "grey"))

;; Setup 3rdparty packages
(leaf arduino-mode
  :doc "Major mode for editing Arduino code"
  :req "emacs-25.1" "spinner-1.7.3"
  :tag "arduino" "languages" "emacs>=25.1"
  :added "2021-01-04"
  :url "https://github.com/stardiviner/arduino-mode"
  :emacs>= 25.1
  :ensure t
  :after spinner
  :mode ("\\.ino$")
  :commands arduino-mode)

(leaf ccls
  :doc "ccls client for lsp-mode"
  :req "emacs-26.1" "lsp-mode-6.3.1" "dash-2.14.1"
  :tag "c++" "lsp" "languages" "emacs>=26.1"
  :added "2021-01-04"
  :url "https://github.com/MaskRay/emacs-ccls"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode
  :custom '((ccls-executable . "/snap/bin/ccls")
            (ccls-sem-highlight-method 'font-lock)
            (ccls-use-default-rainbow-sem-highlight))
  :hook '((c-mode-hook c++-mode-hook objc-mode-hook) lsp))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :blackout t
  :ensure t
  :leaf-defer nil
  :bind (("<C-tab>" . company-complete)
         (company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection)
          ("M-." . company-show-doc-buffer))
         (company-search-map
          ("S-<tab>" . company-select-previous)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :disabled t
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf cmake-mode
  :doc "major-mode for editing CMake sources"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2021-01-04"
  :emacs>= 24.1
  :mode ("\\.cmake" "\\.CMakeLists.txt$")
  :ensure t)

(leaf cuda-mode
  :doc "NVIDIA CUDA Major Mode"
  :tag "languages" "c"
  :added "2021-01-04"
  :mode ("\\.cuh?$")
  :ensure t)

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "emacs>=24"
  :added "2021-01-04"
  :url "https://github.com/spotify/dockerfile-mode"
  :emacs>= 24
  :mode ("\\.Dockerfile$")
  :ensure t)

(leaf dumb-jump
  :doc "Jump to definition for 40+ languages without configuration"
  :req "emacs-24.3" "s-1.11.0" "dash-2.9.0" "popup-0.5.3"
  :tag "programming" "emacs>=24.3"
  :added "2021-01-04"
  :url "https://github.com/jacktasia/dumb-jump"
  :emacs>= 24.3
  :ensure t
  :global-minor-mode dumb-jump-mode
  :bind (("M-d" . dumb-jump-go)
         ("M-S-d" . dumb-jump-back))
  :custom '((dumb-jump-selector . 'ivy)
            (dumb-jump-use-visible-window . nil)))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2021-01-04"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :blackout t
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode
  :custom '((flycheck-gcc-language-standard . "c++17")))

(leaf git-gutter+
  :doc "Manage Git hunks straight from the buffer"
  :req "git-commit-0" "dash-0"
  :tag "vc" "git"
  :added "2021-01-04"
  :url "https://github.com/nonsequitur/git-gutter-plus"
  :ensure t
  :after git-commit
  :global-minor-mode global-git-gutter+-mode)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :blackout t
  :ensure t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-virtual-buffers . t)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2021-01-04"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv
  :commands lsp
  :custom `((lsp-auto-configure . t)
            (lsp-eldoc-enable-hover . t)
            (lsp-eldoc-render-all . t)
            (lsp-enable-completion-at-point . t)
            (lsp-enable-snippet . t)
            (lsp-enable-indentation . nil)
            (lsp-enable-xref . t)
            (lsp-document-sync-method . 'incremental)
            (lsp-file-watch-ignored . '("[/\\\\]\\.venv$"
                                        "[/\\\\]\\.mypy_cache$"
                                        "[/\\\\]__pycache__$"))
            (lsp-file-watch-threshold . nil)
            (lsp-idle-delay . 0.5)
            (lsp-inhibit-message . t)
            (lsp-message-project-root-warning . t)
            (lsp-prefer-capf . t)
            (lsp-prefer-flymake . nil))
  :hook ((prog-major-mode-hook . lsp-prog-major-mode-enable)
         (c-mode-common-hook . lsp-deferred)
         (python-mode-hook . lsp-deferred)))

(leaf lsp-ivy
  :doc "LSP ivy integration"
  :req "emacs-26.1" "dash-2.14.1" "lsp-mode-6.2.1" "ivy-0.13.0"
  :tag "debug" "languages" "emacs>=26.1"
  :added "2021-01-04"
  :url "https://github.com/emacs-lsp/lsp-ivy"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode ivy
  :commands lsp-ivy-workspace-symbol)

(leaf lsp-pyright
  :doc "Python LSP client using Pyright"
  :req "emacs-26.1" "lsp-mode-7.0" "dash-2.18.0" "ht-2.0"
  :tag "lsp" "tools" "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-pyright"
  :added "2021-10-12"
  :emacs>= 26.1
  :ensure t
  ;; :after lsp-mode
  :hook (python-mode-hook . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config
  (unless (executable-find "pyright")
    (unless (executable-find "npm")
      (lsp--error "Both pyright and npm are not installed"))
    (lsp--info "Installing pyright...")
    (lsp-async-start-process
     (lambda ()
       (when (executable-find "pyright")
         (lsp--info "Installing pyright...done")
         (and lsp-mode (lsp))))
     #'lsp--error
     "npm install -g pyright")))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2021-02-14"
  :url "https://github.com/magit/magit"
  :emacs>= 25.1
  :ensure t
  :after git-commit with-editor
  :bind (("C-x g" . magit-status))
  :defvar magit-mode-map
  :config
  (defun magit-open-github-pull-request-url ()
    "Open Github Create Pull-Request page on web browser"
    (interactive)
    (unless (magit-get-push-remote)
      (error "Push to remote first!"))
    (browse-url (format "https://github.com/%s/pull/new/%s"
                        (replace-regexp-in-string
                         "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                         (magit-get "remote"
                                    (magit-get-push-remote)
                                    "url"))
                        (magit-get-current-branch))))
  (define-key magit-mode-map "G" #'magit-open-github-pull-request-url))

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-25.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=25.1"
  :added "2021-01-04"
  :url "https://jblevins.org/projects/markdown-mode/"
  :emacs>= 25.1
  :mode ("\\.md$" "\\.markdown$")
  :ensure t)

(setq-default ros-distro (format "/opt/ros/%s/share/emacs/site-lisp/"
                                 (or (getenv "ROS_DISTRO") "melodic")))
(leaf rosemacs
  :tag "out-of-MELPA"
  :added "2021-01-04"
  :when (file-exists-p ros-distro)
  :load-path ros-distro
  :bind (("C-x C-r" . ros-keymap))
  :require t
  :custom `((ros-topic-update-interval . 0)
            (ros-node-update-interval . 0))
  :init
  (defalias 'dynamic-completion-table 'completion-table-dynamic)
  :config
  (invoke-rosemacs)
  (global-set-key "\C-x\C-r" ros-keymap))

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :added "2021-01-04"
  :url "https://web-mode.org"
  :emacs>= 23.1
  :ensure t
  :mode ("\\.p?html$" "\\.php$" "\\.xml$" "\\.jsx?$" "\\.ejs$")
  :custom `((web-mode-markup-indent-offset . ,tab-width)
            (web-mode-css-indent-offset . ,tab-width)
            (web-mode-code-indent-offset . ,tab-width)))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2021-01-04"
  :emacs>= 24.1
  :ensure t
  :mode "\\.ya?ml$")


(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2021-01-04"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t
  :bind ("<tab>" . yas-keymap)
  :global-minor-mode yas-global-mode)

(leaf yatemplate
  :doc "File templates with yasnippet"
  :req "yasnippet-0.8.1" "emacs-24.3"
  :tag "convenience" "files" "emacs>=24.3"
  :added "2021-01-12"
  :url "https://github.com/mineo/yatemplate"
  :emacs>= 24.3
  :ensure t
  :after yasnippet
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1))
