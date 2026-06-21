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

(defun %initialize-batch-state ()
  "Reset the global shell state for a non-interactive (batch or script) run."
  (setf *running* t
        *last-exit-code* 0
        *last-command-duration-ms* nil
        *environment* (nshell.domain.environment:inject-os-environment
                       (nshell.domain.environment:make-default-environment))
        *aliases* (make-hash-table :test #'equal)
        *abbreviations* (make-hash-table :test #'equal)
        *functions* (make-hash-table :test #'equal)
        *function-sources* (make-hash-table :test #'equal)
        *proc-registry* (make-hash-table :test #'eql))
  (install-expansion-filesystem)
  (configure-completion-filesystem))

(defun run-repl-batch (&key line)
  "Batch (non-interactive) mode: read lines, execute commands, print raw output."
  (%initialize-batch-state)
  (if line
      (handle-batch-line line)
      (loop for input-line = (read-line *standard-input* nil nil)
            while (and input-line *running*)
            do (handle-batch-line input-line)))
  *last-exit-code*)

(defun run-repl-script (path &optional script-args)
  "Execute the script file at PATH (multiline blocks supported, via the same
block-aware reader as the `source' builtin). SCRIPT-ARGS are exposed to the
script as $argv. Returns the exit status of the last command."
  (%initialize-batch-state)
  (handler-case
      (let ((nshell.domain.expansion:*positional-args* script-args))
        (multiple-value-bind (output code)
            (%execute-with-repl-shell-context
             (lambda (context)
               (funcall (nshell.application:lookup-builtin "source")
                        context (list path))))
          (declare (ignore output))
          (setf *last-exit-code* (or code 0))))
    (error (condition)
      (format *error-output* "nshell: ~a~%" condition)
      (setf *last-exit-code* 1)))
  *last-exit-code*)
