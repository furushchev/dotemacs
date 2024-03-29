;; .emacs.d/init.el
;;
;; Author: Yuki Furuta <me@furushchev.jp>
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
            (user-mail-address . "y.furuta@gitai.tech"))
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
  :hook
  (c-mode-common-hook . (lambda () (c-set-style "linux")
                          (setq c-basic-offset tab-width)
                          (c-set-offset 'inline-open 0)
                          (c-set-offset 'inline-close 0)
                          (c-set-offset 'member-init-intro 0)
                          (c-set-offset 'innamespace 0)
                          (c-set-offset 'arglist-intro '++))))

(leaf python-mode
  :doc "Python major mode"
  :tag "oop" "python" "processes" "languages"
  :url "https://gitlab.com/groups/python-mode-devs"
  :added "2022-04-29"
  :ensure t
  :mode ("\\.py$"))

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

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :blackout t
  :ensure t
  :bind ((company-active-map
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

(leaf diff-hl
  :doc "Highlight uncommitted changes using VC"
  :req "cl-lib-0.2" "emacs-25.1"
  :tag "diff" "vc" "emacs>=25.1"
  :url "https://github.com/dgutov/diff-hl"
  :added "2024-03-27"
  :emacs>= 25.1
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "emacs>=24"
  :added "2021-01-04"
  :url "https://github.com/spotify/dockerfile-mode"
  :emacs>= 24
  :mode ("\\.Dockerfile$")
  :ensure t)

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :req "emacs-26.3" "jsonrpc-1.0.16" "flymake-1.2.1" "project-0.9.8" "xref-1.6.2" "eldoc-1.11.0" "seq-2.23" "external-completion-0.1"
  :tag "languages" "convenience" "emacs>=26.3"
  :url "https://github.com/joaotavora/eglot"
  :added "2024-02-22"
  :emacs>= 26.3
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives
                                 '("pylsp"
                                   "jedi-language-server"
                                   ("pyright-langserver" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `((c++-mode c-mode) . ,(eglot-alternatives
                                       '("clangd"
                                         "clangd-10"
                                         "clangd-9"
                                         "clangd-8"
                                         "clangd-7"))))
  :hook ((python-mode-hook . eglot-ensure)
         (c-mode-common-hook . eglot-ensure))
)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2021-01-04"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t)

(leaf expand-region
  :doc "Increase selected region by semantic units."
  :req "emacs-24.4"
  :tag "region" "marking" "emacs>=24.4"
  :url "https://github.com/magnars/expand-region.el"
  :added "2024-03-29"
  :emacs>= 24.4
  :ensure t
  :bind (("C-\\" . er/expand-region))
)

(leaf go-mode
  :doc "Major mode for the Go programming language"
  :req "emacs-26.1"
  :tag "go" "languages" "emacs>=26.1"
  :url "https://github.com/dominikh/go-mode.el"
  :added "2023-04-04"
  :emacs>= 26.1
  :ensure t)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2024-03-27"
  :emacs>= 24.5
  :blackout t
  :ensure t
  :global-minor-mode t
  :config
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t)
)

(leaf swiper
  :doc "Isearch with an overview.  Oh, man!"
  :req "emacs-24.5" "ivy-0.14.2"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2024-03-27"
  :emacs>= 24.5
  :blackout t
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward))
)

(leaf counsel
  :doc "Various completion functions using Ivy"
  :req "emacs-24.5" "ivy-0.14.2" "swiper-0.14.2"
  :tag "tools" "matching" "convenience" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :added "2024-03-29"
  :emacs>= 24.5
  :ensure t
  :after ivy swiper
  :bind (("M-x" . counsel-M-x)
         ("C-c C-d" . counsel-dired)
         ("C-c C-f" . counsel-find-file)
         ("C-c C-g" . counsel-git)
         ("C-c C-g" . counsel-git-grep)
         ("C-c C-a" . counsel-ag)
         ("C-c C-l" . counsel-locate))
)

(leaf js2-mode
  :doc "Improved JavaScript editing mode"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "javascript" "languages" "emacs>=24.1"
  :url "https://github.com/mooz/js2-mode/"
  :added "2024-03-27"
  :emacs>= 24.1
  :ensure t
  :mode "\\.js$")

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
  :req "emacs-27.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=27.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2024-02-22"
  :emacs>= 27.1
  :ensure t
  :mode ("\\.md$" "\\.markdown$")
)

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1"
  :tag "convenience" "project" "emacs>=25.1"
  :url "https://github.com/bbatsov/projectile"
  :added "2024-03-27"
  :emacs>= 25.1
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)

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
  :mode ("\\.p?html$" "\\.php$" "\\.xml$" "\\.jsx?$" "\\.ejs$" "\\.json$")
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
