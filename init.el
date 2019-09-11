;; .emacs.d/init.el
;;
;; Author: Yuki Furuta <furushchev@jsk.imi.i.u-tokyo.ac.jp>
;;

(defun user-full-name ()
  "Yuki Furuta")

(defun user-mail-address ()
  "me@furushchev.ru")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expand gc threshold at start time
(defconst initial-gc-cons-threshold gc-cons-threshold
  "initial value of gc-conc-threshold at start-up time")
(setq gc-cons-threshold (* 100 gc-cons-threshold))
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

;; disable bell
(setq ring-bell-function 'ignore)

;; suppress warnings
(setq enable-local-variables :safe)

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
 '(display-time-mode nil)
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function
   (function flycheck-display-error-messages-unless-error-list))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Editings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; backup in ~/.emacs.d/backup
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
             `("\\.*$" . ,(expand-file-name (concat user-emacs-directory "backup"))))

;; auto-save files in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name (concat user-emacs-directory "backup") t))))

;; keep new line on file end
(setq require-final-newline t)

;; use soft tabs (2 spaces)
(setq-default indent-tabs-mode nil)
(setq-default tab-width       2
              c-basic-offset  2
              js-indent-level 2)
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent-offset 4)))
(add-hook 'c-mode-common-hook '(lambda ()
                                 (c-set-style "linux")
                                 (setq c-basic-offset tab-width)
                                 (c-set-offset 'inline-open 0)
                                 (c-set-offset 'inline-close 0)
                                 (c-set-offset 'member-init-intro 0)
                                 (c-set-offset 'innamespace 0)
                                 (c-set-offset 'arglist-intro 4)
                                 ))
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

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "site-lisp")))

;; roswell
(when (file-exists-p (expand-file-name "~/.roswell/helper.el"))
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq slime-lisp-implementations
        `((ros ("ros" "run"))
          (sbcl ("sbcl"))
          (abcl ("abcl"))
          (clisp ("clisp")))))

;; el-get for keep backward compatibility
(cond
 ((version< emacs-version "24.4")
  (load (expand-file-name (concat user-emacs-directory "init-24.3.el")) t))
 (t
  (load (expand-file-name (concat user-emacs-directory "init-latest.el")) t)))

;; automatically make directory
(add-hook 'find-file-not-found-functions
          #'(lambda ()
              (let ((dir (file-name-directory (buffer-file-name))))
                (make-directory dir t)
                nil)))

;; el-doc
(setq-default eldoc-idle-delay 0.1
              eldoc-echo-area-use-multiline-p t)
(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook #'eldoc-mode))
(when (require 'diminish nil 'noerror)
  (diminish 'eldoc-mode))

;; abbrev
(setq-default abbrev-mode t)
(when (require 'diminish nil 'noerror)
  (diminish 'abbrev-mode))

;; auto revert
(global-auto-revert-mode t)
(setq-default auto-revert-verbose nil
              global-auto-revert-non-file-buffers t)

;; save place
(require 'saveplace)
(setq-default save-place t
              save-place-file (concat user-emacs-directory "places"))

;; use directory name instead of <num>
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(put 'dired-find-alternate-file 'disabled nil)

;; rosemacs
(setq-default ros/distro (or (getenv "ROS_DISTRO") "kinetic"))
(add-to-list 'load-path (format "/opt/ros/%s/share/emacs/site-lisp" ros/distro))
(require 'rosemacs-config nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; 3rdparty libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'euslisp-mode nil 'noerror)
