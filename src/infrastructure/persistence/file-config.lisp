(in-package #:nshell.infrastructure.persistence)
(defun config-file-path () (merge-pathnames ".nshellrc" (user-homedir-pathname)))
(defun load-config () nil)
(defun save-config (config) (declare (ignore config)) nil)
