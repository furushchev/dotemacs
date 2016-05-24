;;; init.el
;;; Author: Yuki Furuta

(require 'cl)

;; add site-lisp to load-path
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path)))

;; expand gc threshold at start time
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "initial value of gc-conc-threshold at start-up time")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;; initialize package manager
(require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; install minimum 3rdparty packages
(let* ((package-list
        '(init-loader
          use-package))
       (pkgs-not-installed
        (loop for p in package-list
              when (not (package-installed-p p))
              collect p)))
  (when pkgs-not-installed
    (package-refresh-contents)
    (dolist (pkg pkgs-not-installed)
      (package-install pkg))))

;; profiling init script
(require 'init-profiling)
(add-hook 'after-init-hook
          (lambda ()
            (message "Initialize completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

;; load settings from ~/.emacs.d/init
(require 'use-package)
(setq init-loader-show-log-after-init nil)
(init-loader-load (expand-file-name "~/.emacs.d/init"))
