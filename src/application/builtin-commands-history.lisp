;;; History builtin implementation and helpers
(in-package #:nshell.application)

(defparameter +history-search-option-specs+
  '(("--prefix" :mode :prefix)
    ("--contains" :mode :contains)
    ("--exact" :mode :exact)
    ("--case-sensitive" :case-sensitive t)))

(defparameter +history-subcommand-specs+
  '(("search" :handler %history-search)
    ("delete" :handler %history-delete)
    ("clear" :handler %history-clear)
    ("size" :handler %history-size)))

(defun %history-usage ()
  (%builtin-usage
   "history"
   (%builtin-usage-clauses-summary +builtin-history-usage-clauses+)))

(defun %history-format-entries (entries)
  (when entries
    (with-output-to-string (out)
      (dolist (entry entries)
        (format out "~a~%" (nshell.domain.history:entry-text entry))))))

(defun %history-subcommand-spec (command)
  (cdr (assoc command +history-subcommand-specs+ :test #'string=)))

(defun %history-search-options (args)
  (labels ((parse (remaining mode case-sensitive)
             (let ((spec (and remaining
                              (cdr (assoc (first remaining)
                                          +history-search-option-specs+
                                          :test #'string=)))))
               (if spec
                   (parse (rest remaining)
                          (or (getf spec :mode) mode)
                          (or case-sensitive (getf spec :case-sensitive)))
                   (values mode case-sensitive remaining)))))
    (parse args :contains nil)))

(defun %history-list (history)
  (values (%history-format-entries
           (reverse (nshell.domain.history:history-all history)))
          0))

(defun %history-search (history args)
  (multiple-value-bind (mode case-sensitive query-parts)
      (%history-search-options args)
    (if query-parts
        (values
         (%history-format-entries
          (nshell.domain.history:history-search
           history (%join-command-args query-parts)
           :mode mode
           :case-sensitive case-sensitive
           :smartcase (not case-sensitive)))
         0)
        (values (%history-usage) 1))))

(defun %history-delete (history args)
  (if args
      (let ((deleted (nshell.domain.history:history-delete
                      history (%join-command-args args))))
        (values (format nil "~d~%" deleted) 0))
      (values (%history-usage) 1)))

(defun %history-clear (history args)
  (declare (ignore args))
  (nshell.domain.history:history-clear history)
  (values nil 0))

(defun %history-size (history args)
  (declare (ignore args))
  (values (format nil "~d~%" (nshell.domain.history:history-size history)) 0))

(defun %builtin-history (context args)
  (let ((history (shell-context-history context)))
    (if (null args)
        (%history-list history)
        (let ((spec (%history-subcommand-spec (first args))))
          (if spec
              (funcall (getf spec :handler) history (rest args))
              (values (%history-usage) 1))))))
