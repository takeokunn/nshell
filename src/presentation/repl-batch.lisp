;;; Batch REPL execution
(in-package #:nshell.presentation)

(defun handle-batch-line (line)
  (handler-case
      (nshell.domain.parsing:with-parsed-command-line-case (result ast line)
        (:complete
         (sync-exported-environment)
         (setf *last-exit-code* (or (execute-ast ast) 0)))
        (:error
         (report-parse-diagnostics result *error-output*)
         (setf *last-exit-code* 2))
        (:incomplete
         (report-parse-diagnostics result *error-output*)
         (setf *last-exit-code* 2)))
    (error (condition)
      (format *error-output* "nshell error: ~a~%" condition)
      (setf *last-exit-code* 1))))

(defun run-repl-batch ()
  "Batch (non-interactive) mode: read lines, execute commands, print raw output."
  (setf *running* t
        *last-exit-code* 0
        *environment* (nshell.domain.environment:inject-os-environment
                       (nshell.domain.environment:make-default-environment))
        *aliases* (make-hash-table :test #'equal)
        *abbreviations* (make-hash-table :test #'equal)
        *functions* (make-hash-table :test #'equal)
        *proc-registry* (make-hash-table :test #'eql))
  (install-expansion-filesystem)
  (configure-completion-filesystem)
  (loop for line = (read-line *standard-input* nil nil)
        while (and line *running*)
        do (handle-batch-line line))
  *last-exit-code*)
