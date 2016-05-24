(defvar ros/distro (getenv "ROS_DISTRO"))

(defun ros/version>= (ver1 ver2)
  (and (stringp ver1) (stringp ver2)
       (>= (elt ver1 0) (elt ver2 0))))

(when (stringp ros/distro)
  (cond
   ((ros/version>= ros/distro "indigo") ;; indigo and later
    (use-package rosemacs-config
      :load-path (format nil "/opt/ros/~A/share/emacs/site-lisp" ros/distro))
   (t ;; hydro and former
    (use-package rosemacs
      :config
      (invoke-rosemacs)
      :bind ("C-x C-r" . ros-keymap)))))
