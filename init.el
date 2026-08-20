;; .emacs.d/init.el
;;
;; Author: Yuki Furuta <furushchev@gmail.com>
;;

;; Setup leaf.el
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))

  ;; Emacs <28 bundles the 2019 GNU ELPA signing key, which expired 2024-04-21,
  ;; and gnu-elpa-keyring-update cannot repair it because gpg 2.2 rejects the
  ;; replacement key ("new key but contains no user ID - skipped").  Without
  ;; this, archive-contents fails verification and the whole GNU ELPA archive is
  ;; silently discarded -- 0 of 504 packages visible -- which makes compat,
  ;; spinner, jsonrpc and the rest permanently uninstallable.
  (when (< emacs-major-version 28)
    (customize-set-variable 'package-check-signature nil))

  ;; `leaf-handler-package' reacts to a failed install by calling
  ;; `package-refresh-contents' and retrying -- up to two refreshes per failing
  ;; :ensure, on every startup.  For a dependency that can never be satisfied
  ;; that is an unbounded cost: this config was measured at 16.85s of startup,
  ;; 12.32s of it in 60 HTTP requests, against a 0.25s floor.  Keep ordinary
  ;; startups off the network entirely; install explicitly instead:
  ;;   EMACS_INSTALL_PACKAGES=1 emacs
  (defconst my/package-install-allowed (and (getenv "EMACS_INSTALL_PACKAGES") t)
    "Non-nil when this session is permitted to install packages.")
  (unless my/package-install-allowed
    (advice-add 'package-refresh-contents :override #'ignore)
    (advice-add 'package-install :override
                (lambda (&rest _)
                  (error "Package install skipped; re-run with EMACS_INSTALL_PACKAGES=1"))))

  (package-initialize)
  (when (< emacs-major-version 26)
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

;; Pinned packages
;;
;; Upstream has moved the packages below to Emacs 28.1/29.1, and no ELPA archive
;; (gnu, nongnu, melpa, melpa-stable) retains older versions -- each keeps only
;; the newest.  So on Emacs 27.1 they are unobtainable via package.el at any
;; version, and every :ensure t for them failed on each startup.  Pin each to
;; the last upstream release that still declares (emacs "27.1") or lower and
;; fetch it from git with el-get, which clones the tag, byte-compiles and
;; generates autoloads.  Installed cost is ~0.2s; nothing here touches the
;; network once cloned.
;;
;; Ordering matters: el-get byte-compiles in sequence, so a package must come
;; after anything it requires at compile time.
;;
;; When Emacs is newer than 27.1 this whole block can be deleted and the
;; corresponding leaf blocks returned to plain :ensure t.
(defconst my/pinned-packages
  '((:name transient      :type git :load-path ("lisp")
           :url "https://github.com/magit/transient.git"        :checkout "v0.10.1")
    (:name with-editor    :type git :load-path ("lisp")
           :url "https://github.com/magit/with-editor.git"      :checkout "v3.5.0")
    ;; magit v4.4.0 raised its floor to Emacs 28.1; v4.3.8 also supplies
    ;; magit-section, so one clone covers both.
    (:name magit          :type git :load-path ("lisp")
           :url "https://github.com/magit/magit.git"            :checkout "v4.3.8")
    (:name corfu          :type git :load-path ("." "extensions")
           :url "https://github.com/minad/corfu.git"            :checkout "1.5")
    (:name popon          :type git
           :url "https://codeberg.org/akib/emacs-popon.git"     :checkout "v0.13")
    ;; corfu-terminal requires corfu, which package.el cannot resolve here
    ;; because our corfu is pinned outside the archives -- so pin this too.
    (:name corfu-terminal :type git
           :url "https://codeberg.org/akib/emacs-corfu-terminal.git" :checkout "v0.7")
    (:name cape           :type git
           :url "https://github.com/minad/cape.git"             :checkout "1.7")
    (:name vertico        :type git :load-path ("." "extensions")
           :url "https://github.com/minad/vertico.git"          :checkout "1.9")
    (:name consult        :type git
           :url "https://github.com/minad/consult.git"          :checkout "1.8")
    (:name marginalia     :type git
           :url "https://github.com/minad/marginalia.git"       :checkout "1.7")
    ;; embark 1.1 also supplies embark-consult, so it must follow consult.
    (:name embark         :type git
           :url "https://github.com/oantolin/embark.git"        :checkout "1.1")
    (:name projectile     :type git
           :url "https://github.com/bbatsov/projectile.git"     :checkout "v2.9.1")
    (:name markdown-mode  :type git
           :url "https://github.com/jrblevin/markdown-mode.git" :checkout "v2.7"))
  "Recipes for packages pinned to their last Emacs 27.1-compatible release.")

;; Dependencies of the pinned packages that package.el *can* still satisfy on
;; Emacs 27.1.  These must precede the el-get block: el-get byte-compiles each
;; pinned package as it clones it, so they have to be loadable by then.
(leaf compat :doc "Compatibility shims required by consult, embark, vertico, magit"
  :ensure t :require t)
(leaf llama  :doc "Required by magit 4.3.8" :ensure t)
(leaf seq    :doc "magit 4.3.8 needs seq 2.24; Emacs 27.1 bundles 2.21"
  :ensure t)

(leaf el-get
  :doc "Fetch pinned packages from git at a fixed tag"
  :ensure t
  :require t
  :custom `((el-get-dir . ,(locate-user-emacs-file "el-get/"))
            (el-get-notify-type . 'message))
  :config
  (setq el-get-sources my/pinned-packages)
  ;; Activate what is already cloned.  Cloning only happens when installs are
  ;; explicitly enabled, so a network outage can never slow down a normal start.
  (dolist (recipe my/pinned-packages)
    (let ((name (plist-get recipe :name)))
      (when (or my/package-install-allowed
                (el-get-package-is-installed name))
        (condition-case err
            (el-get 'sync name)
          (error (display-warning 'init (format "pinned package %s: %s"
                                                name (error-message-string err)))))))))

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
  :mode ("\\.py$"))

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
  ;; Unobtainable on Emacs 27.1: every tagged release, back to the oldest
  ;; (v0.0.3), requires Emacs 29.1, so there is nothing to pin.  The guard below
  ;; makes leaf skip this block outright rather than retry the install.
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
  :require t                                   ; pinned to 1.5, see my/pinned-packages
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
  ;; corfu-popupinfo, corfu-history and corfu-info live in corfu's extensions/
  ;; subdirectory, which el-get puts on `load-path' (see :load-path in
  ;; `my/pinned-packages') but does not always scrape autoloads from: on Emacs 29
  ;; it generates el-get/.loaddefs.el from the package root only, so
  ;; `corfu-popupinfo-mode' is void there while it is autoloaded fine on 27.1.
  ;; Requiring the features explicitly makes this independent of that.
  (require 'corfu-popupinfo)
  (require 'corfu-history)
  (require 'corfu-info)                        ; supplies the M-d / M-l commands
  (global-corfu-mode 1)
  (corfu-popupinfo-mode)                       ; Show documentation popup
  (corfu-history-mode))                        ; Remember completion history

(leaf popon
  :doc "\"Pop\" floating text \"on\" a window (dependency for corfu-terminal)"
  :url "https://codeberg.org/akib/emacs-popon"
  :added "2026-01-14"
  :require t)                                  ; pinned to v0.13, see my/pinned-packages

(leaf corfu-terminal
  :doc "Terminal support for Corfu (required for non-GUI Emacs)"
  :req "emacs-26.1" "corfu-0.7" "popon-0.13"
  :tag "convenience" "emacs>=26.1"
  :url "https://codeberg.org/akib/emacs-corfu-terminal"
  :added "2026-01-14"
  :emacs>= 26.1
  :after corfu popon
  :require t                                   ; pinned to v0.7, see my/pinned-packages
  :defer-config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

(leaf cape
  :doc "Completion At Point Extensions - enhances CAPF for LSP"
  :req "emacs-27.1"
  :tag "completion" "convenience" "emacs>=27.1"
  :url "https://github.com/minad/cape"
  :added "2026-01-13"
  :emacs>= 27.1                                ; pinned to 1.7, see my/pinned-packages
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
               `((c++-mode c-mode c++-ts-mode c-ts-mode) . ,(eglot-alternatives
                                                              '("clangd"
                                                                "clangd-20"
                                                                "clangd-19"
                                                                "clangd-18"
                                                                "clangd-10"
                                                                "clangd-9"
                                                                "clangd-8"
                                                                "clangd-7"))))

  :hook ((python-mode-hook . eglot-ensure)
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
  :emacs>= 27.1                                ; pinned to 1.8, see my/pinned-packages
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
  :emacs>= 27.1                                ; pinned to 1.1, see my/pinned-packages
  :blackout t
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
  :emacs>= 27.1                                ; ships with pinned embark 1.1
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
  :emacs>= 25.1                                ; pinned to v4.3.8, see my/pinned-packages
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
  :emacs>= 27.1                                ; pinned to 1.7, see my/pinned-packages
  :init (marginalia-mode))

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-27.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=27.1"
  :url "https://jblevins.org/projects/markdown-mode"
  :added "2024-02-22"
  :emacs>= 27.1                                ; pinned to v2.7, see my/pinned-packages
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
  :emacs>= 25.1                                ; pinned to v2.9.1, see my/pinned-packages
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
  :emacs>= 27.1                                ; pinned to 1.9, see my/pinned-packages
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
