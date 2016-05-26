;; author
(setq user-full-name "Yuki Furuta")
(setq user-mail-address "furushchev@jsk.imi.i.u-tokyo.ac.jp")

;; backup in ~/.emacs.d/backup
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
             `("\\.*$" . ,(expand-file-name "~/.emacs.d/backup/")))

;; auto-save files in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;; comment out
(setq comment-style 'multi-line)

;; highlight parenthenesses
(setq show-paren-delay 0)
(show-paren-mode t)

;; keep new line on file end
(setq require-final-newline t)

;; always revert buffer
(global-auto-revert-mode 1)

;; y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; indentation
(add-hook 'c-mode-common-hook '(lambda ()
                                 (c-set-style "linux")
                                 (setq indent-tabs-mode nil)
                                 (setq c-basic-offset tab-width)))
(setq-default
 default-tab-width 2
 js-indent-level 2
 tab-width 2
 indent-tabs-mode nil)

;; performance
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-time 0.2 nil #'linum-update-current))

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

;; comment
(global-set-key "\C-cc" 'comment-region)    ; C-c c を範囲指定コメントに
(global-set-key "\C-cu" 'uncomment-region)  ; C-c u を範囲指定コメント解除に

;; disable splash/scratch
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; workaround for inputting backslash on Cocoa Emacs
(when (eq system-type 'darwin)
  (define-key global-map [?\¥] [?\\])
  (define-key global-map [?\C-¥] [?\C-\\])
  (define-key global-map [?\M-¥] [?\M-\\])
  (define-key global-map [?\C-\M-¥] [?\C-\M-\\]))

;; chmod +x if there is shebang on file top
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
