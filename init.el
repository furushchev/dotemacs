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
(setq default-enable-multibyte-characters t)

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
;;;;;; el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-get-lock
(el-get-bundle tarao/el-get-lock
  (el-get-lock)
  (el-get-lock-unlock 'el-get))

;; ag
(el-get-bundle ag)

;; cmake-mode
(el-get-bundle cmake-mode)

;; git
(el-get-bundle git-gutter-fringe+)

;; irony
(el-get-bundle irony-eldoc)
(el-get-bundle irony-mode
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'c++-mode-hook 'irony-mode))

;; jedi
(el-get-bundle jedi
  (load "~/.emacs.d/subr-x.el")
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:use-shortcuts t))

;; markdown-mode
(el-get-bundle markdown-mode)

;; popwin
(el-get-bundle popwin)

;; slime
(el-get-bundle slime)

;; web-mode
(el-get-bundle web-mode
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode)))

;; yaml-mode
(el-get-bundle yaml-mode)

;; yatemplate
(el-get-bundle yatemplate
  (yatemplate-fill-alist)
  (auto-insert-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; rosemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default ros/distro (or (getenv "ROS_DISTRO") "indigo"))
(add-to-list 'load-path
             (format "/opt/ros/%s/share/emacs/site-lisp" ros/distro))
(require 'rosemacs-config)
(put 'dired-find-alternate-file 'disabled nil)
