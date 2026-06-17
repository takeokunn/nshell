(in-package #:nshell.presentation)

(defun %execute-background-ast (ast)
  (cond
    ((nshell.domain.parsing:command-node-p ast)
     (multiple-value-bind (cmd redirects)
         (%prepare-command-node ast)
       (let ((proc (nshell.infrastructure.acl:spawn-async
                    (nshell.domain.parsing:command-node-command cmd)
                    (mapcar #'nshell.domain.parsing:arg-value
                            (nshell.domain.parsing:command-node-args cmd))
                    :redirects redirects)))
         (when proc
           (%register-background-job ast proc)))))
    ((nshell.domain.parsing:pipeline-node-p ast)
     (multiple-value-bind (cmds redirects)
         (%prepare-pipeline-node ast)
        (let ((procs (nshell.infrastructure.acl:spawn-pipeline-async
                     cmds
                     :redirects redirects)))
         (when procs
           (%register-background-job ast procs)))))
    (t
     (format *error-output* "nshell: cannot run construct in background~%"))))

(defun %prepare-command-node (cmd)
  (multiple-value-bind (args redirects)
      (extract-redirects
       (expand-arg-list
        (nshell.domain.parsing:command-node-args cmd)))
    (values (nshell.domain.parsing:make-command-node
             (nshell.domain.parsing:command-node-command cmd)
             args)
            redirects)))

(defun %prepare-pipeline-node (pipeline)
  (let ((pipeline-redirects nil))
    (values
     (loop for cmd in (nshell.domain.parsing:pipeline-node-commands pipeline)
           collect (multiple-value-bind (clean-cmd redirects)
                       (%prepare-command-node cmd)
                     (push redirects pipeline-redirects)
                     clean-cmd))
     (nreverse pipeline-redirects))))

(defun %ast-job-text (ast)
  (cond
    ((nshell.domain.parsing:command-node-p ast)
     (nshell.domain.parsing:command-node-command ast))
    ((nshell.domain.parsing:pipeline-node-p ast)
     (format nil "~{~a~^ | ~}"
             (mapcar #'nshell.domain.parsing:command-node-command
                     (nshell.domain.parsing:pipeline-node-commands ast))))
    (t "")))

(defun %register-background-job (ast procs)
  (let* ((proc-list (if (listp procs) procs (list procs)))
         (pids (mapcar #'sb-ext:process-pid proc-list))
         (pgid (first pids))
         (job (make-job-from-ast ast (%ast-job-text ast)))
         (jid (nshell.domain.job-control:monitor-add-job
               nshell.application:*job-monitor* job)))
    (setf (nshell.domain.execution:job-pids job) pids)
    (setf (nshell.domain.execution:job-pgid job) pgid)
    (setf (nshell.domain.execution:job-background-p job) t)
    (nshell.domain.job-control:monitor-update
     nshell.application:*job-monitor* jid :running)
    (register-background-proc jid procs)
    (format t "[~d] ~d~%" jid pgid)))
