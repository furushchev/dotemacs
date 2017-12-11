;; .emacs.d/init.el
;;
;; Author: Yuki Furuta <furushchev@jsk.imi.i.u-tokyo.ac.jp>
;;

(defun user-mail-address ()
  "furushchev@jsk.imi.i.u-tokyo.ac.jp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expand gc threshold at start time
(defconst initial-gc-cons-threshold gc-cons-threshold
  "initial value of gc-conc-threshold at start-up time")
(setq gc-cons-threshold (* 10 gc-cons-threshold))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold initial-gc-cons-threshold)))

;; delay calculate line number
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-time 0.2 nil #'linum-update-current))

;; disable splash/scratch
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; disable right to left languages
(setq-default bidi-display-reordering nil)

;; ignore warning on byte-compile
(setq byte-compile-warnings
      '(not nresolved free-vars callargs redefine obsolete noruntime cl-functions interactive-only))

;; chmod +x if there is shebang on file top
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; comment
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; C-h as backspace
(global-set-key (kbd "C-h") 'backward-delete-char)

;; M-g as goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; triple ESC as quit
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)

;; move cursor to *occur*
(defun occur-and-select (regexp &optional nlines)
  (interactive (occur-read-primary-args))
  (occur regexp nlines)
  (select-window (get-buffer-window "*Occur*"))
  (forward-line 1))
(global-set-key (kbd "M-s o") 'occur-and-select)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Appearances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; back slash instead of yen
(define-key global-map [?¥] [?\\])

;; highlight parenthenesses
(setq show-paren-delay 0)
(show-paren-mode t)

;; show cursor pos
(column-number-mode t)
(line-number-mode t)

;; warn whitespaces
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "red"))) nil)
(defface my-face-u-1 '((t (:background "red"))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("\t" 0 my-face-b-1 append)
     ("　" 0 my-face-b-2 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; hide menu bar on top
(custom-set-variables
 '(blink-matching-paren t)
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Editings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; backup in ~/.emacs.d/backup
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
             `("\\.*$" . ,(expand-file-name "~/.emacs.d/backup/")))

;; auto-save files in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;; keep new line on file end
(setq require-final-newline t)

;; always revert buffer
(global-auto-revert-mode 1)

;; use soft tabs (2 spaces)
(setq-default indent-tabs-mode nil)
(setq-default tab-width       2
              c-basic-offset  2
              js-indent-level 2)
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent-offset 4)))
(add-hook 'c-mode-common-hook '(lambda ()
                                 (c-set-style "linux")
                                 (setq c-basic-offset tab-width)))
(add-hook 'sh-mode-hook '(lambda ()
                           (setq sh-basic-offset tab-width
                                 sh-indentation tab-width)))

;; encodings
(set-language-environment  'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'sjis-mac)
(set-buffer-file-coding-system 'utf-8-unix)
(when (not file-name-coding-system)
  (set-file-name-coding-system 'utf-8))
(setq locale-coding-system 'utf-8)
(global-font-lock-mode t)
(set-buffer-multibyte t)

;; automatically make directory
(add-hook 'find-file-not-found-functions
          #'(lambda ()
              (let ((dir (file-name-directory (buffer-file-name))))
                (make-directory dir t)
                nil)))

;; use directory name instead of <num>
(require 'uniquify nil 'noerror)
(setq uniquify-buffer-name-style 'forward)

;; save last cursor place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; el-doc
(setq-default eldoc-idle-delay 0.1
              eldoc-echo-area-use-multiline-p t)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook #'eldoc-mode))

;; writable dired
(require 'wdired)
(setq wdired-allow-to-change-permissions t)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package diminish :ensure t)
(require 'bind-key)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; cmake-mode
(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'" "CMakeLists.txt"))

;; company
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)))

(el-get-bundle company-quickhelp
  (eval-after-load 'company-mode
    (add-hook 'global-company-mode-hook 'company-quickhelp-mode)))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
   '(flycheck-display-errors-delay 0.5)))

(use-package flycheck-pos-tip
  :requires (flycheck pos-tip)
  :ensure t
  :defer t
  :config
  (flycheck-pos-tip-mode))

(el-get-bundle git-gutter-fringe+)

(use-package google-c-style
  :ensure t
  :hook ((c-mode . google-set-c-style)
         (c++-mode . google-set-c-style))
  :config
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package irony
  :ensure t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :requires irony-mode
  :hook (irony-mode . irony-eldoc))

(use-package jedi
  :ensure t
  :disabled ;; use el-get-bundle for emacs>=24.3
  :pin melpa-stable
  :hook (python-mode . jedi:setup)
  :config
  (load "~/.emacs.d/subr-x.el")
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t))

(el-get-bundle jedi
  (load "~/.emacs.d/subr-x.el")
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package company-jedi
  :disabled ;; use el-get-bundle for emacs>=24.3
  :ensure t
  :pin melpa-stable
  :requires (jedi company)
  :config
  (add-to-list 'company-backends 'company-jedi))

(el-get-bundle company-jedi
  (add-to-list 'company-backends 'company-jedi))

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.md\\'"))

(use-package popwin
  :ensure t
  :commands popwin-mode)

(use-package pos-tip
  :ensure t)

(use-package python-mode
  :ensure t
  :disabled
  :mode "\\.pyx?\\'")

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
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/el-get/yasnippet/snippets"))
  (define-key yas-keymap (kbd "<tab>") nil)
  (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; rosemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default ros/distro (or (getenv "ROS_DISTRO") "indigo"))
(add-to-list 'load-path
             (format "/opt/ros/%s/share/emacs/site-lisp" ros/distro))
(require 'rosemacs-config nil t)
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  euswank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slime
(use-package slime
  :ensure t
  :commands slime
  :config
  (use-package slime-quicklisp
    :ensure t
    :config
    (slime-setup
     '(slime-fancy
       slime-quicklisp
       ;; slime-euswank
       ))
    ;; (setq slime-lisp-implementations
    ;;       '((euswank ("euswank") :coding-system utf-8-unix)))
    )
  )

