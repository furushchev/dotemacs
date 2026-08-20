;; .emacs.d/early-init.el -*- lexical-binding: t; -*-
;;
;; Loaded before the GUI is initialised and before package.el runs.
;;
;; The `gc-cons-threshold' set via :custom in init.el cannot speed up
;; initialisation itself -- by the time custom applies it, init.el has already
;; finished consing.  Raise it here instead, and restore a sane value afterwards
;; so that long editing sessions do not accumulate a huge, pause-prone heap.

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Every `load' and `require' during startup otherwise walks this list of
;; regexps, none of which can match while we are only loading local .el files.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 40000000 ; keep in sync with init.el's :custom
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist))
          ;; Append so this runs after anything else on the hook.
          t)

;; init.el calls `package-initialize' itself inside `eval-and-compile'.
(setq package-enable-at-startup nil)
