;;; validate-startup.el --- Load this config the way a real Emacs does -*- lexical-binding: t; -*-
;;
;; Run as:  emacs --batch -l .github/ci/validate-startup.el
;;
;; `emacs --batch -l init.el' -- the obvious thing, and what the old CI did --
;; is not a faithful startup, in two ways that both hide real breakage:
;;
;;   1. Batch mode never loads early-init.el, so any interaction between the
;;      two files goes untested.  early-init.el raising `gc-cons-threshold' to
;;      `most-positive-fixnum' once made init.el's `(* 50 gc-cons-threshold)'
;;      overflow, which took out that entire `leaf' block -- invisibly, because
;;      of (2).
;;
;;   2. `leaf' wraps each block in a `condition-case' and downgrades a failure
;;      to `Warning (leaf)'.  A block can fail completely -- dropping every
;;      setting in it -- while Emacs still exits 0.  Exit status alone is
;;      therefore not a validation.
;;
;; So: load both files in the real order, run `emacs-startup-hook' the way
;; startup.el would, and treat anything logged to *Warnings* as a failure.

;; `string-trim' / `string-empty-p' live in subr-x on Emacs 27.
(require 'subr-x)

(setq debug-on-error t)

;; init.el sets `display-warning-minimum-level' to :error, which suppresses
;; *showing* the warning buffer.  Logging is governed separately; pin it so a
;; config change can never quietly stop warnings from reaching us.
(setq warning-minimum-log-level :warning)

(defconst validate/installing (and (getenv "EMACS_INSTALL_PACKAGES") t)
  "Non-nil when this run is allowed to reach the network.")

;; A normal startup must not touch the network: init.el neutralises
;; `package-refresh-contents' and `package-install' unless
;; EMACS_INSTALL_PACKAGES is set, because a single unsatisfiable :ensure
;; otherwise costs a refresh-and-retry storm on every single start.  That
;; guarantee is worth asserting rather than trusting -- trip on the actual
;; fetch, so any new network caller is caught too, not just package.el's.
(unless validate/installing
  (dolist (fn '(url-retrieve url-retrieve-synchronously))
    (advice-add fn :override
                (lambda (&rest args)
                  (error "Offline startup tried to reach the network: %s %S"
                         fn (car args))))))

(let* ((early (locate-user-emacs-file "early-init.el"))
       (init  (locate-user-emacs-file "init.el"))
       (start (current-time)))
  (dolist (f (list early init))
    (unless (file-exists-p f)
      (princ (format "FAIL: expected config file is missing: %s\n" f))
      (kill-emacs 1)))

  (load early nil t)
  (load init nil t)
  ;; startup.el runs this after init.el; early-init.el hangs its GC and
  ;; `file-name-handler-alist' restoration on it, so skipping it would leave
  ;; the session in a state no real Emacs is ever in.
  (run-hooks 'emacs-startup-hook)

  (let* ((elapsed (float-time (time-subtract (current-time) start)))
         (buf (get-buffer "*Warnings*"))
         (warnings (and buf (with-current-buffer buf
                              (string-trim (buffer-string))))))
    (princ (format "\nEmacs %s loaded the config in %.2fs\n"
                   emacs-version elapsed))

    ;; Spot-check that the blocks whose failure is otherwise silent actually
    ;; took effect.  A trapped `leaf' error shows up here as a default value.
    (princ (format "  gc-cons-threshold ... %s\n" gc-cons-threshold))
    (princ (format "  tab-width ......... %s\n" tab-width))
    (princ (format "  custom-file ....... %s\n" custom-file))

    (when (and warnings (not (string-empty-p warnings)))
      (princ "\nFAIL: startup logged warnings (a trapped `leaf' error drops\n")
      (princ "      every setting in its block, so these are not cosmetic):\n\n")
      (princ warnings)
      (princ "\n")
      (kill-emacs 1))

    (princ "\nOK: config loaded with no warnings.\n")))

;;; validate-startup.el ends here
