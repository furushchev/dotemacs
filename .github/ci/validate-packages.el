;;; validate-packages.el --- Prove the declared packages actually load -*- lexical-binding: t; -*-
;;
;; Run as:  emacs --batch -l .github/ci/validate-packages.el
;;
;; Startup alone does not prove much about packages.  Nearly every block in
;; init.el is lazy -- `:ensure t' with a `:mode' or `:bind' only registers an
;; autoload -- so a package can be half-installed, or byte-compiled against
;; the wrong version of a dependency, and a clean startup will still say
;; nothing about it.  package.el compiling eglot against a newer `project' on
;; Emacs 27.1, for instance, prints an error at install time and exits 0.
;;
;; This loads the config and then `require's each package the config declares,
;; which is the point at which such a mismatch actually surfaces.

(require 'subr-x)

(setq warning-minimum-log-level :warning)

(defconst validate/packages
  '(arduino-mode
    cape
    cmake-mode
    consult
    corfu
    cuda-mode
    diff-hl
    eglot
    el-get
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
the ones a broken pin or a stale archive would take out.")

(defconst validate/packages-29
  '(bazel)
  "Packages init.el gates behind `:emacs>= 29.1'.
Unobtainable on 27.1 -- every tagged emacs-bazel-mode release requires 29.1 --
so init.el skips the block there and so do we.")

(let ((early (locate-user-emacs-file "early-init.el"))
      (init  (locate-user-emacs-file "init.el")))
  (load early nil t)
  (load init nil t)
  (run-hooks 'emacs-startup-hook))

(let ((wanted (append validate/packages
                      (when (or (> emacs-major-version 29)
                                (and (= emacs-major-version 29)
                                     (>= emacs-minor-version 1)))
                        validate/packages-29)))
      (failures 0))
  (princ (format "\nRequiring %d package(s) on Emacs %s:\n"
                 (length wanted) emacs-version))
  (dolist (pkg wanted)
    (condition-case err
        (progn
          (require pkg)
          (princ (format "  ok    %s\n" pkg)))
      (error
       (setq failures (1+ failures))
       (princ (format "  FAIL  %s: %s\n" pkg (error-message-string err))))))
  (princ (format "\n%d package(s) required, %d failure(s)\n"
                 (length wanted) failures))
  (kill-emacs (if (> failures 0) 1 0)))

;;; validate-packages.el ends here
