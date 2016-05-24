(defvar ros/distro (getenv "ROS_DISTRO"))

(defun ros/version>= (ver1 ver2)
  (and (stringp ver1) (stringp ver2)
       (>= (elt ver1 0) (elt ver2 0))))

(when (stringp ros/distro)
  (cond
   ((ros/version>= ros/distro "indigo") ;; indigo and later
    (add-to-list 'load-path
                 (format nil "/opt/ros/~A/share/emacs/site-lisp" ros/distro))
    (when (locate-library 'rosemacs-config)
      (require 'rosemacs-config)))
   (t ;; hydro and former
    (when (locate-library 'rosemacs)
      (require 'rosemacs)
      (invoke-rosemacs)
      (global-set-key "C-x C-r" ros-keymap)))))
