(setq debug-on-error t)
(load (expand-file-name "./init.el"))
(defun show-log ()
  (print "\n\n---------- error log -----------")
  (print (init-loader-error-log))
  (print "\n\n---------- init log ------------")
  (print (init-loader-log))
  (print "\n\n------- load path -------")
  (print (mapconcat 'identity load-path "\n")))
(show-log)