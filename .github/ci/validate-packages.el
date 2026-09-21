;;; validate-packages.el --- Prove the declared packages actually load -*- lexical-binding: t; -*-
;;
;; Run as:  emacs --batch -l .github/ci/validate-packages.el
;;
;; Startup alone does not prove much about packages.  Nearly every block in
;; init.el is lazy -- `:ensure t' with a `:mode' or `:bind' only registers an
;; autoload -- so a package can be half-installed, or byte-compiled against
;; the wrong version of a dependency, and a clean startup will still say
;; nothing about it.  Requiring each feature catches incomplete dependency
;; resolution and byte-compilation failures that startup alone can miss.
;;
;; This loads the config and then `require's each package the config declares,
;; which is the point at which such a mismatch actually surfaces.

(require 'subr-x)

(setq warning-minimum-log-level :warning)

(defconst validate/packages
  '(arduino-mode
    bazel
    cape
    cmake-mode
    consult
    corfu
    corfu-terminal
    cuda-mode
    diff-hl
    eglot
    embark
    embark-consult
    exec-path-from-shell
    expand-region
    go-mode
    js2-mode
    leaf
    magit
    marginalia
    markdown-mode
    orderless
    popon
    projectile
    protobuf-mode
    python-mode
    systemrdl-mode
    vertico
    web-mode
    yaml-mode
    yasnippet
    yatemplate)
  "Packages init.el declares that should be loadable after startup.
Deliberately the top-level set, not every transitive dependency: these are
the ones broken package resolution or a stale archive would take out.")

(let ((early (locate-user-emacs-file "early-init.el"))
      (init  (locate-user-emacs-file "init.el")))
  (load early nil t)
  (load init nil t)
  (run-hooks 'emacs-startup-hook))

(let ((failures 0))
  (princ (format "\nRequiring %d package(s) on Emacs %s:\n"
                 (length validate/packages) emacs-version))
  (dolist (pkg validate/packages)
    (condition-case err
        (progn
          (require pkg)
          (princ (format "  ok    %s\n" pkg)))
      (error
       (setq failures (1+ failures))
       (princ (format "  FAIL  %s: %s\n" pkg (error-message-string err))))))
  (princ (format "\n%d package(s) required, %d failure(s)\n"
                 (length validate/packages) failures))
  (kill-emacs (if (> failures 0) 1 0)))

;;; validate-packages.el ends here
