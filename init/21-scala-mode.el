(require 'scala-mode)
(require 'scala-mode-auto)
(require 'scala-mode-feature-electric-mode)
(add-hook 'scala-electric-mode-hook '(lambda () (autopair-mode -1)))
