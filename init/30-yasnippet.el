(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-expand yas-insert-snippet)
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets"))
  (yas-global-mode 1)
  (custom-set-variables '(yas-trigger-key "TAB"))
  (setf yas/indent-line nil)
  (setf (symbol-function 'yas-active-keys)
        (lambda ()
          (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables))))))
