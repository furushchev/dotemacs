;;; init.el
;;; Author: Yuki Furuta

(require 'cl)

;; add site-lisp to load-path
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; initialize package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar package-list
  '(
    ace-jump-mode
    auto-complete
    ac-slime
    bash-completion
    dockerfile-mode
    euslisp-mode
    exec-path-from-shell
    flycheck
    google-c-style
    go-mode
    go-autocomplete
    haskell-mode
;;    helm
    init-loader
    jedi
    js2-mode
    markdown-mode
    projectile
    ruby-mode
    scala-mode
    slime
    ssh-config-mode
    yaml-mode
    yasnippet
    ))

;; install 3rdparty packages
(let ((pkgs-not-installed
       (loop for p in package-list
	     when (not (package-installed-p p))
	     collect p)))
  (when pkgs-not-installed
    (package-refresh-contents)
    (dolist (pkg pkgs-not-installed)
      (package-install pkg))))


(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/init")
