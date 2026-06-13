(in-package #:nshell.infrastructure.persistence)
(defvar *history-file-path-override* nil
  "When non-nil, overrides the default history file path. Used for testing.")

(defun history-file-path ()
  "Return the history file path. Respects *history-file-path-override* for testing."
  (or *history-file-path-override*
      (merge-pathnames ".nshell_history" (user-homedir-pathname))))
(defun load-history-file ()
  (handler-case
      (let ((path (history-file-path)))
        (when (probe-file path)
          (with-open-file (f path :direction :input :if-does-not-exist nil)
            (loop for line = (read-line f nil nil)
                  while line collect line))))
    (error () nil)))
(defun append-history-entry (text)
  (handler-case
      (progn
        (ensure-directories-exist (history-file-path))
        (with-open-file (f (history-file-path) :direction :output
                           :if-exists :append :if-does-not-exist :create)
          (format f "~a~%" text)))
    (error () nil)))
(defun vacuum-history (max-entries)
  (declare (ignore max-entries))
  t)
