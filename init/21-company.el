(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  ;; select candidate by Tab if it is only one.
  (defun company--insert-candidate2 (candidate)
    (when (> (length candidate) 0)
      (setq candidate (substring-no-properties candidate))
      (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
          (insert (company-strip-prefix candidate))
        (if (equal company-prefix candidate)
            (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
          (insert candidate))
        )))
  (defun company-complete-common2 ()
    (interactive)
    (when (company-manual-begin)
      (if (and (not (cdr company-candidates))
               (equal company-common (car company-candidates)))
          (company-complete-selection)
        (company--insert-candidate2 company-common))))
  (define-key company-active-map [tab] 'company-complete-common2)
  (define-key company-active-map [backtab] 'company-select-previous)
  ;; set color
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "white")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
  :bind
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))
