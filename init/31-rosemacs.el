(defvar rosdistro (getenv "ROS_DISTRO"))

(defun rosversion>= (ver1 ver2)
  (>= (elt ver1 0) (elt ver2 0)))

(when rosdistro
  (cond
   ((rosversion>= rosdistro "indigo") ;; indigo and later
    (add-to-list 'load-path
                 (format nil "/opt/ros/~A/share/emacs/site-lisp" rosdistro))
    (when (locate-library 'rosemacs-config)
      (require 'rosemacs-config))
   (t ;; hydro and former
    (when (locate-library 'rosemacs)
      (require 'rosemacs)
      (invoke-rosemacs)
      (global-set-key "\C-x\C-r" ros-keymap))))))
