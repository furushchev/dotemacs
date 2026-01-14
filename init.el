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
    ;; WARNING: Signature verification disabled for Emacs <26
    ;; Only use with trusted package sources
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
  :custom `((user-full-name . ,(or (getenv "USER_FULL_NAME") "Yuki Furuta"))
            (user-login-name . ,(or (getenv "USER_LOGIN_NAME") "furushchev"))
            (user-mail-address . ,(or (getenv "USER_EMAIL") "y.furuta@gitai.tech"))))

(leaf custom-edit
  :doc "Custom variables for editing"
  :tag "builtin"
  :custom `((context-menu-mode . t)
            (custom-file . ,(locate-user-emacs-file "custom.el"))
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
            (minibuffer-prompt-properties . '(read-only t cursor-intangible-mode t face minibuffer-prompt))
            (read-extended-command-predicate . #'command-completion-default-include-p)
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
            (initial-scratch-message . nil)
            (line-number-mode . t)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (scroll-bar-mode . nil)
            (scroll-conservatively . 100)
            (scroll-preserve-screen-position . t)
            (show-paren-delay . 0)
            (show-paren-mode . t)
            (text-quoting-style . 'straight)))

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

(leaf savehist
  :doc "Save minibuffer history"
  :tag "builtin"
  :added "2025-06-14"
  :init (savehist-mode))

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
                          (setq indent-tabs-mode nil)
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

(leaf treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode))
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.21.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (make . ("https://github.com/alemuller/tree-sitter-make" "a4b9187"))
               (elisp . ("https://github.com/Wilfred/tree-sitter-elisp" "v1.3.0"))
               (cmake . ("https://github.com/uyha/tree-sitter-cmake" "v0.4.1"))
               (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.21.3"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Remap legacy modes to tree-sitter modes
  ;; Note: This does *not* extend to hooks! Migrate hooks separately
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (sh-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(leaf whitespace
  :doc "minor mode to visualize HARD TAB, ZENKAKU SPACE"
  :tag "builtin"
  :added "2021-01-04"
  :custom '((whitespace-space-regexp . "\\(\u3000+\\)")
            (whitespace-style . '(face tabs tab-mark spaces space-mark))
            (whitespace-display-mappings . '()))
  :hook ((prog-mode-hook . whitespace-mode)
         (text-mode-hook . whitespace-mode)
         (conf-mode-hook . whitespace-mode))
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
  :doc "Modular text completion framework - DISABLED in favor of Corfu (2026-01-13)"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :blackout t
  :ensure t
  :when nil  ; Disabled: set to t to re-enable
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
  :hook ((prog-mode-hook . company-mode)
         (text-mode-hook . company-mode)))

(leaf corfu
  :doc "Completion Overlay Region FUnction - modern completion UI"
  :req "emacs-27.1"
  :tag "completion" "convenience" "emacs>=27.1"
  :url "https://github.com/minad/corfu"
  :added "2026-01-13"
  :emacs>= 27.1
  :ensure t
  :custom ((corfu-auto . t)                    ; Auto-show completions
           (corfu-cycle . t)                   ; Cycle through candidates
           (corfu-auto-delay . 0.0)            ; No delay for auto-completion
           (corfu-auto-prefix . 2)             ; Minimum prefix length
           (corfu-popupinfo-delay . 0.5)       ; Documentation popup delay
           (corfu-preview-current . t)         ; Preview current candidate
           (corfu-preselect . 'prompt)         ; Preselect behavior
           (corfu-on-exact-match . nil))       ; Don't auto-complete on exact match
  :bind (:corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("RET" . corfu-insert)
         ("M-d" . corfu-show-documentation)
         ("M-l" . corfu-show-location))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)                       ; Show documentation popup
  (corfu-history-mode))                        ; Remember completion history

(leaf cape
  :doc "Completion At Point Extensions - enhances CAPF for LSP"
  :req "emacs-27.1"
  :tag "completion" "convenience" "emacs>=27.1"
  :url "https://github.com/minad/cape"
  :added "2026-01-13"
  :emacs>= 27.1
  :ensure t
  :init
  ;; Cache buster for LSP servers to continuously update candidates
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :config
  ;; Add useful completion-at-point backends
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

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
  :hook ((prog-mode-hook . diff-hl-mode)
         (dired-mode-hook . diff-hl-dired-mode))
  :config
  (diff-hl-margin-mode))

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :req "emacs-26.3" "jsonrpc-1.0.16" "flymake-1.2.1" "project-0.9.8" "xref-1.6.2" "eldoc-1.11.0" "seq-2.23" "external-completion-0.1"
  :tag "languages" "convenience" "emacs>=26.3"
  :url "https://github.com/joaotavora/eglot"
  :added "2024-02-22"
  :emacs>= 26.3
  :ensure t
  :custom ((eglot-autoshutdown . t)            ; Shutdown server when last buffer closes
           (eglot-sync-connect . nil)          ; Async connection
           (eglot-events-buffer-size . 0))     ; Disable event logging (performance)
  :config
  ;; Python LSP server configuration with formatters and linters
  ;; Respects .style.yapf, .isort.cfg, pylintrc automatically
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins
                          (:yapf (:enabled t)                        ; Enable yapf formatter
                           :isort (:enabled t)                       ; Enable isort import sorting
                           :pylint (:enabled t :args [])             ; Enable pylint linter
                           :flake8 (:enabled :json-false)            ; Disable flake8
                           :autopep8 (:enabled :json-false)          ; Disable autopep8 (prefer yapf)
                           :pycodestyle (:enabled t :maxLineLength 88) ; Style checking
                           :pydocstyle (:enabled :json-false)))))    ; Disable docstring style

  ;; Language server programs
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

  ;; Optional: Format-on-save helper
  (defun my/eglot-format-on-save-mode ()
    "Enable format-on-save for Eglot-managed buffers."
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))

  ;; Enable format-on-save for specific languages (uncomment to enable)
  ;; (add-hook 'python-ts-mode-hook #'my/eglot-format-on-save-mode)
  ;; (add-hook 'c-ts-mode-hook #'my/eglot-format-on-save-mode)
  ;; (add-hook 'c++-ts-mode-hook #'my/eglot-format-on-save-mode)

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

(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "files" "matching" "emacs>=28.1"
  :url "https://github.com/minad/consult"
  :added "2025-06-14"
  :emacs>= 28.1
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-s" . consult-line)
         ;; Disabled: consult-find conflicts with default find-file workflow
         ;; ("C-x C-f" . consult-find)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-project-buffer)
         ("M-g" . consult-goto-line)
         ("M-o" . consult-imenu)
         ("M-s s" . consult-git-grep)))

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

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "help" "docs" "emacs>=28.1"
  :url "https://github.com/minad/marginalia"
  :added "2025-06-14"
  :emacs>= 28.1
  :ensure t
  ;; :after compat
  :init (marginalia-mode))

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-27.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=27.1"
  :url "https://jblevins.org/projects/markdown-mode"
  :added "2024-02-22"
  :emacs>= 27.1
  :ensure t
  :mode ("\\.md$" "\\.markdown$"))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-27.1" "compat-30"
  :tag "completion" "matching" "emacs>=27.1"
  :url "https://github.com/oantolin/orderless"
  :added "2025-06-14"
  :emacs>= 27.1
  :ensure t
  ;; :after compat
  :custom '((completion-styles . '(orderless basic))
            (completion-category-defaults . nil)
            (completion-category-overrides . '((file (styles partial-completion))))))

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

(leaf protobuf-mode
  :doc "Major mode for editing protocol buffers"
  :tag "languages" "protobuf" "google"
  :url "https://github.com/protocolbuffers/protobuf"
  :added "2025-04-28"
  :ensure t
  :mode ("\\.proto")
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

(leaf systemrdl-mode
  :tag "out-of-MELPA"
  :added "2025-06-23"
  :load-path `,(expand-file-name "site-lisp/systemrdl-mode" user-emacs-directory)
  :require t
  :mode ("\\.rdl"))

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "files" "convenience" "emacs>=28.1"
  :url "https://github.com/minad/vertico"
  :added "2025-06-14"
  :emacs>= 28.1
  :ensure t
  ;; :after compat
  :init (vertico-mode))

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
  :mode ("\\.ya?ml$" "\\.repos$"))

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
