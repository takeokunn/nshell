(in-package #:nshell.application)

(defun %read-stream-to-string (stream)
  (with-output-to-string (out)
    (loop for char = (read-char stream nil nil)
          while char
          do (write-char char out))))

(defun execute-command-line (line history dispatcher)
  (nshell.domain.parsing:with-complete-command-line (result ast line)
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-command-entered-event line)))
    (nshell.domain.history:history-add history line)
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-command-appended-to-history-event line)))
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-command-parsed-event ast)))
    (values ast result)))

(defun execute-pipeline (pipeline-ast)
  "Execute a pipeline AST using OS-level pipes through the infrastructure layer."
  (let ((commands (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
                      (nshell.domain.parsing:pipeline-node-commands pipeline-ast)
                      (list pipeline-ast))))
    (multiple-value-bind (clean-commands redirects)
        (%extract-pipeline-redirects commands)
      (nshell.infrastructure.acl:spawn-pipeline clean-commands
                                                :redirects redirects))))

(defun execute-pipeline-use-case (pipeline dispatcher)
  (when dispatcher
    (publish-event dispatcher
                   (nshell.domain.events:make-pipeline-started-event pipeline nil)))
  (let ((exit-code (or (execute-pipeline pipeline) 0)))
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-pipeline-completed-event pipeline exit-code)))
    exit-code))

(defun %extract-command-redirects (cmd-node)
  (let ((clean nil)
        (redirects nil)
        (args (nshell.domain.parsing:command-node-args cmd-node)))
    (loop with index = 0
          with limit = (length args)
          while (< index limit)
          for arg = (nth index args)
          for value = (nshell.domain.parsing:arg-value arg)
          for spec = (assoc value nshell.domain.parsing:+redirect-specs+ :test #'string=)
          do (if (and spec (< (1+ index) limit))
                 (let ((target (nshell.domain.parsing:arg-value (nth (1+ index) args))))
                   (push (cons (cdr spec) target) redirects)
                   (incf index 2))
                 (progn
                   (push arg clean)
                   (incf index))))
    (values (nshell.domain.parsing:make-command-node
             (nshell.domain.parsing:command-node-command cmd-node)
             (nreverse clean))
            (nreverse redirects))))

(defun %extract-pipeline-redirects (commands)
  (let ((clean-commands nil)
        (redirects nil))
    (dolist (command commands)
      (multiple-value-bind (clean-command command-redirects)
          (%extract-command-redirects command)
        (push clean-command clean-commands)
        (push command-redirects redirects)))
    (values (nreverse clean-commands) (nreverse redirects))))

(defun %input-redirect-target (redirects)
  (cdr (find :< redirects :key #'car :from-end t)))

(defun %output-redirect-spec (redirects)
  (let ((redirect (find-if (lambda (redirect)
                             (member (car redirect) '(:> :>>)))
                           redirects
                           :from-end t)))
    (when redirect
      (values (cdr redirect)
              (if (eq (car redirect) :>>) :append :supersede)))))

(defun %write-redirected-stage-output (redirects output)
  (multiple-value-bind (target mode)
      (%output-redirect-spec redirects)
    (when target
      (with-open-file (stream target
                              :direction :output
                              :if-exists mode
                              :if-does-not-exist :create)
        (write-string (or output "") stream))
      t)))
