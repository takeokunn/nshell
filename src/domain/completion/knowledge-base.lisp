(in-package #:nshell.domain.completion)
(defstruct (knowledge-base (:constructor make-knowledge-base ()))
  (commands (make-hash-table :test #'equal) :type hash-table))
(defun kb-add-command (kb cmd-name &key subcommands flags description)
  (setf (gethash cmd-name (knowledge-base-commands kb))
        (list :subcommands subcommands
              :flags flags
              :description description)))
(defun kb-add-option (kb cmd-name opt-name)
  (let ((entry (gethash cmd-name (knowledge-base-commands kb))))
    (when entry (pushnew opt-name (getf entry :flags) :test #'string=))))
(defun kb-query (kb cmd-name)
  (gethash cmd-name (knowledge-base-commands kb)))
