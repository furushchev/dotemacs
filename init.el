;; .emacs.d/init.el -*- lexical-binding: t; -*-
;;
;; Author: Yuki Furuta <furushchev@gmail.com>
;;

;; Setup leaf.el
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    :config
    (leaf-keywords-init)
    (leaf blackout :ensure t)))

(leaf leaf
  :doc "Install leaf-convert after leaf is enabled"
  :config
  (leaf leaf-convert :ensure t))

;; Setup variables
(leaf custom-keybinding
  :doc "Custom variables for keybinding"
  :tag "builtin"
  :bind (("C-c c" . comment-region)
         ("C-c u" . uncomment-region)
         ;; M-g is bound by the consult block below, which loads later and wins;
         ;; binding goto-line here as well was dead weight.
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
            (debug-on-error . nil)
            (display-warning-minimum-level . :error)
            (enable-local-variables . :safe)
            (enable-recursive-minibuffers . t)
            (frame-resize-pixelwise . t)
            (gc-cons-threshold . 40000000)  ; Keep in sync with early-init.el.
            (history-delete-duplicates . t)
            (history-length . 1000)
            (indent-tabs-mode . nil)
            (init-file-debug . nil)
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
  :mode ("\\.py$")
  :preface
  (defun my/python-mode-disable-process-completion ()
    "Remove python-mode's process-backed completion from this buffer.

The external python-mode package installs `py-fast-complete' as a
completion-at-point function.  Corfu calls it automatically, which starts an
inferior Python process and displays a separate completions buffer.  Eglot and
Cape provide the completion-at-point functions used by this configuration.

Returns:
  The updated buffer-local completion-at-point function list."
    (setq-local completion-at-point-functions
                (delq #'py-fast-complete completion-at-point-functions)))
  :hook (python-mode-hook . my/python-mode-disable-process-completion))

(leaf sh-mode
  :doc "Shell mode properties"
  :tag "builtin"
  :mode ("\\.sh$" "\\.bash$" "\\.zsh$")
  :custom `((sh-basic-offset . ,tab-width)))

;; (leaf treesit-auto
;;   :doc "Automatically install and use tree-sitter grammars"
;;   :req "emacs-29.0"
;;   :tag "treesitter" "languages" "emacs>=29.0"
;;   :url "https://github.com/renzmann/treesit-auto"
;;   :added "2026-01-14"
;;   :emacs>= 29.0
;;   :ensure t
;;   :require t
;;   :custom ((treesit-auto-install . 'prompt))  ; Prompt before installing grammars
;;   :config
;;   (global-treesit-auto-mode))

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

(leaf bazel
  :doc "Bazel support for Emacs"
  :req "emacs-29.1"
  :tag "languages" "build tools" "emacs>=29.1"
  :url "https://github.com/bazelbuild/emacs-bazel-mode"
  :added "2026-01-14"
  :emacs>= 29.1
  :ensure t
  :mode ("\\.bazel$" "\\.bzl" "BUILD" "MODULE" "WORKSPACE" "REPO" "\\.bazelrc$"))

(leaf corfu
  :doc "Completion Overlay Region FUnction - modern completion UI"
  :req "emacs-27.1"
  :tag "completion" "convenience" "emacs>=27.1"
  :url "https://github.com/minad/corfu"
  :added "2026-01-13"
  :emacs>= 27.1
  :ensure t
  :require t
  :custom ((corfu-auto . t)                    ; Auto-show completions
           (corfu-cycle . t)                   ; Cycle through candidates
           (corfu-auto-delay . 0.2)            ; No delay for auto-completion
           (corfu-auto-prefix . 2)             ; Minimum prefix length
           (corfu-popupinfo-delay . 0.5)       ; Documentation popup delay
           (corfu-preview-current . t)         ; Preview current candidate
           (corfu-preselect . 'prompt)         ; Preselect behavior
           (corfu-on-exact-match . nil))       ; Don't auto-complete on exact match
  :bind ((corfu-map
          ("RET" . corfu-expand)
          ("<tab>" . corfu-next)
          ("S-<tab>" . corfu-previous)
          ("<backtab>" . corfu-previous)
          ("M-d" . corfu-info-documentation)
          ("M-l" . corfu-info-location)))
  :config
  (global-corfu-mode 1)
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
  :mode ("\\.cmake$" "CMakeLists\\.txt$")
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
  :preface
  (defconst my/python-lsp-project-directory
    (expand-file-name (locate-user-emacs-file "python-lsp/"))
    "Directory containing the locked Python LSP project.")

  (defconst my/python-lsp-venv-directory
    (expand-file-name (locate-user-emacs-file "venv/"))
    "Directory containing the uv-managed Python LSP environment.")

  (defconst my/python-lsp-executable
    (expand-file-name "bin/pylsp" my/python-lsp-venv-directory)
    "Absolute path to the uv-managed pylsp executable.")

  (defconst my/python-lsp-lock-file
    (expand-file-name "uv.lock" my/python-lsp-project-directory)
    "Lock file defining the Python LSP environment.")

  (defconst my/python-lsp-ready-file
    (expand-file-name ".emacs-python-lsp-ready" my/python-lsp-venv-directory)
    "Stamp written after the Python LSP environment is synchronized.")

  (defvar my/python-lsp-setup-process nil
    "Process currently synchronizing the Python LSP environment.")

  (defvar my/python-lsp-pending-buffers nil
    "Python buffers waiting for the Python LSP environment.")

  (defun my/python-lsp-project-environment ()
    "Find a Python environment in the current project root.

Returns:
  The absolute path to a project Python executable, or nil when neither
  `.venv' nor `venv' contains one."
    (let ((candidates
           (list (expand-file-name ".venv/bin/python" default-directory)
                 (expand-file-name "venv/bin/python" default-directory)))
          found)
      (while (and candidates (not found))
        (when (file-executable-p (car candidates))
          (setq found (car candidates)))
        (setq candidates (cdr candidates)))
      found))

  (defun my/python-lsp-workspace-configuration (_server)
    "Build pylsp configuration for the current project.

Args:
  _SERVER: The Eglot server requesting configuration.  It is unused because
    Eglot binds `default-directory' to the project root before calling this
    function.

Returns:
  A plist suitable for `eglot-workspace-configuration'."
    (let ((plugins
           '(:yapf (:enabled t)
             :isort (:enabled t)
             :pylint (:enabled t :args [])
             :flake8 (:enabled :json-false)
             :autopep8 (:enabled :json-false)
             :pycodestyle (:enabled t :maxLineLength 88)
             :pydocstyle (:enabled :json-false)))
          (environment (my/python-lsp-project-environment)))
      (when environment
        (setq plugins
              (append plugins `(:jedi (:environment ,environment)))))
      `(:pylsp (:plugins ,plugins))))

  (defun my/python-lsp-ready-p ()
    "Return whether the locked Python LSP environment is ready.

Returns:
  Non-nil when pylsp is executable and the ready stamp is at least as new as
  the lock file."
    (and (file-executable-p my/python-lsp-executable)
         (file-readable-p my/python-lsp-lock-file)
         (file-exists-p my/python-lsp-ready-file)
         (not (file-newer-than-file-p my/python-lsp-lock-file
                                      my/python-lsp-ready-file))))

  (defun my/python-lsp-write-ready-file ()
    "Record that the locked Python LSP environment was synchronized.

Returns:
  The result of writing the ready stamp."
    (make-directory my/python-lsp-venv-directory t)
    (with-temp-file my/python-lsp-ready-file
      (insert (format "Synchronized %s\n" (format-time-string "%FT%T%z")))))

  (defun my/python-lsp-start-eglot (buffer)
    "Start Eglot in BUFFER after the Python LSP setup completes.

Args:
  BUFFER: A Python buffer that was waiting for pylsp.

Returns:
  The return value of `eglot-ensure', or nil when BUFFER is no longer a live
  Python buffer."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'python-mode)
          (eglot-ensure)))))

  (defun my/python-lsp-setup-sentinel (process _event)
    "Handle completion of the uv synchronization PROCESS.

Args:
  PROCESS: The uv process synchronizing the LSP environment.
  _EVENT: The process status event string, which is not otherwise needed.

Returns:
  Nil after handling the process status."
    (when (memq (process-status process) '(exit signal))
      (setq my/python-lsp-setup-process nil)
      (if (and (eq (process-status process) 'exit)
               (zerop (process-exit-status process))
               (file-executable-p my/python-lsp-executable))
          (let ((buffers my/python-lsp-pending-buffers)
                (output-buffer (process-buffer process)))
            (setq my/python-lsp-pending-buffers nil)
            (my/python-lsp-write-ready-file)
            (when (buffer-live-p output-buffer)
              (kill-buffer output-buffer))
            (message "Python LSP environment is ready")
            (dolist (buffer buffers)
              (my/python-lsp-start-eglot buffer)))
        (setq my/python-lsp-pending-buffers nil)
        (display-warning
         'python-lsp
         (format "Python LSP setup failed (exit %s); see *Python LSP Setup*"
                 (process-exit-status process))
         :error)
        (when (buffer-live-p (process-buffer process))
          (display-buffer (process-buffer process)))))
    nil)

  (defun my/python-lsp-start-setup ()
    "Synchronize the locked Python LSP environment asynchronously.

Returns:
  The running uv process, an existing setup process, or nil if setup could not
  be started."
    (cond
     ((process-live-p my/python-lsp-setup-process)
      my/python-lsp-setup-process)
     ((not (file-readable-p my/python-lsp-lock-file))
      (display-warning
       'python-lsp
       (format "Python LSP lock file is missing: %s" my/python-lsp-lock-file)
       :error)
      nil)
     ((not (executable-find "uv"))
      (display-warning 'python-lsp "uv is required to install pylsp" :error)
      nil)
     (t
      (let* ((uv (executable-find "uv"))
             (output-buffer (get-buffer-create "*Python LSP Setup*"))
             (process-environment (copy-sequence process-environment)))
        (setenv "UV_PROJECT_ENVIRONMENT" my/python-lsp-venv-directory)
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "$ UV_PROJECT_ENVIRONMENT=%s %s sync --locked\n\n"
                            my/python-lsp-venv-directory uv))))
        (condition-case err
            (setq my/python-lsp-setup-process
                  (make-process
                   :name "python-lsp-setup"
                   :buffer output-buffer
                   :command (list uv "sync"
                                  "--project" my/python-lsp-project-directory
                                  "--locked" "--no-dev" "--no-progress")
                   :noquery t
                   :sentinel #'my/python-lsp-setup-sentinel))
          (error
           (with-current-buffer output-buffer
             (goto-char (point-max))
             (insert (format "\nFailed to start uv: %s\n"
                             (error-message-string err))))
           (display-warning
            'python-lsp
            (format "Could not start Python LSP setup: %s"
                    (error-message-string err))
            :error)
           (display-buffer output-buffer)
           nil))))))

  (defun my/python-lsp-ensure ()
    "Start Eglot or arrange to start it after pylsp is installed.

Returns:
  The return value of `eglot-ensure', the uv setup process, or nil."
    (if (my/python-lsp-ready-p)
        (eglot-ensure)
      (add-to-list 'my/python-lsp-pending-buffers (current-buffer))
      (my/python-lsp-start-setup)))
  :custom ((eglot-autoshutdown . t)            ; Shutdown server when last buffer closes
           (eglot-sync-connect . nil)          ; Async connection
           (eglot-events-buffer-size . 0))     ; Disable event logging (performance)
  :config
  ;; Python LSP server configuration with formatters and linters
  ;; Respects .style.yapf, .isort.cfg, pylintrc automatically
  (setq-default eglot-workspace-configuration
                #'my/python-lsp-workspace-configuration)

  ;; Language server programs
  (add-to-list 'eglot-server-programs
               `(python-mode . (,my/python-lsp-executable)))
  (add-to-list 'eglot-server-programs
               `((c++-mode c-mode c++-ts-mode c-ts-mode) . ,(eglot-alternatives
                                                              '("clangd"
                                                                "clangd-20"
                                                                "clangd-19"
                                                                "clangd-18"
                                                                "clangd-10"
                                                                "clangd-9"
                                                                "clangd-8"
                                                                "clangd-7"))))

  :hook ((python-mode-hook . my/python-lsp-ensure)
         (c-mode-hook . eglot-ensure)
         (c++-mode-hook . eglot-ensure)
         (c-ts-mode-hook . eglot-ensure)
         (c++-ts-mode-hook . eglot-ensure))
)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2021-01-04"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :config
  ;; Without this the package is inert: a GUI Emacs inherits the desktop
  ;; session's PATH, so eglot cannot find pylsp or clangd.
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  :bind (;; Disabled: consult-line cannot cycle the next candidate
         ;; ("C-s" . consult-line)
         ;; Disabled: consult-find conflicts with default find-file workflow
         ;; ("C-x C-f" . consult-find)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-project-buffer)
         ("M-g" . consult-goto-line)
         ("M-o" . consult-imenu)
         ("M-s s" . consult-git-grep)))

(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-28.1" "compat-30"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2026-01-14"
  :emacs>= 28.1
  :blackout t
  :ensure t
  :require t
  :bind (("C-u" . embark-act)
         ("C-;" . embark-dwim))
  :config
  (setq-default prefix-help-command #'embark-prefix-help-command))

(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-28.1" "compat-30" "embark-1.1" "consult-1.8"
  :tag "convenience" "emacs>=28.1"
  :url "https://github.com/oantolin/embark"
  :added "2026-01-14"
  :emacs>= 28.1
  :ensure t
  :require t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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
  :mode ("\\.rdl"))

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-28.1" "compat-30"
  :tag "completion" "matching" "files" "convenience" "emacs>=28.1"
  :url "https://github.com/minad/vertico"
  :added "2025-06-14"
  :emacs>= 28.1
  :ensure t
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
