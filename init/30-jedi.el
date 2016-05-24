(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq-default jedi:complete-on-dot t)
