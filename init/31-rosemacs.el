(defvar ros/distro (getenv "ROS_DISTRO"))

(defun ros/version>= (ver1 ver2)
  (and (stringp ver1) (stringp ver2)
       (>= (elt ver1 0) (elt ver2 0))))

(when (stringp ros/distro)
  (cond
   ((ros/version>= ros/distro "indigo") ;; indigo and later
    (add-to-list 'load-path (format "/opt/ros/%s/share/emacs/site-lisp" ros/distro))
    (use-package rosemacs-config)
    (add-to-list 'load-path (format "/opt/ros/%s/share/slime_ros" ros/distro))
    (use-package slime-config))
   (t ;; hydro and former
    (use-package rosemacs
      :config
      (invoke-rosemacs)
      :bind ("C-x C-r" . ros-keymap)))))
