(use-package magit
  :load-path "site-lisp/magit/"
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))
