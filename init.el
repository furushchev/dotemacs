;; .emacs.d/init.el
;;
;; Author: Yuki Furuta <me@furushchev.ru>
;;

;; load custom variables
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; setup straight.el
(setq package-enable-at-startup nil
      straight-use-package-by-default t)
(defvar bootstrap-version)
(defvar straight-repository-user "raxod502")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(load (expand-file-name "init-common" user-emacs-directory))
(load (expand-file-name
         (format "init-%s" emacs-major-version) user-emacs-directory) t)
(load (expand-file-name
       (format "init-%s.%s" emacs-major-version emacs-minor-version)
       user-emacs-directory) t)
