(use-package gitignore-mode
  :load-path "site-lisp/git-modes/"
  :mode ("gitignore\\'" . gitignore-mode))
(use-package gitconfig-mode
  :load-path "site-lisp/git-modes/"
  :mode ("gitconfig\\'" . gitconfig-mode))
(use-package gitattributes-mode
  :load-path "site-lisp/git-modes/"
  :mode ("gitattributes\\'" . gitattributes-mode))
(use-package git-commit-mode
  :load-path "site-lisp/git-modes/")
(use-package git-rebase-mode
  :load-path "site-lisp/git-modes/")
