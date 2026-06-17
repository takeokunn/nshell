(in-package #:nshell.presentation)

(defun %repl-filesystem-fns ()
  (list :cwd #'uiop:getcwd
        :list-dir (lambda (dir) (uiop:directory-files dir))
        :chdir #'uiop:chdir
        :stat #'probe-file
        :file-exists-p (lambda (path)
                         (let ((pathname (probe-file path)))
                           (and pathname
                                (not (uiop:directory-pathname-p pathname)))))
        :directory-exists-p (lambda (path)
                              (not (null (uiop:directory-exists-p path))))))

(defun %repl-process-fns ()
  (list :run-external
        (lambda (command args)
          (nshell.infrastructure.acl:run-external command args))
        :run-external-capture
        (lambda (command args)
          (nshell.infrastructure.acl:run-external-capture command args))))

(defun %repl-redirect-fns ()
  (list :redirect-output #'nshell.infrastructure.acl:redirect-output
        :redirect-input #'nshell.infrastructure.acl:redirect-input
        :restore #'nshell.infrastructure.acl:restore-redirects))

(defun %make-repl-shell-context ()
  (nshell.application:make-shell-context
   :history *history*
   :config *config*
   :knowledge-base *kb*
   :environment (ensure-environment)
   :dispatcher nil
   :job-monitor nshell.application:*job-monitor*
   :alias-table *aliases*
   :abbreviation-table *abbreviations*
   :function-table *functions*
   :filesystem-fns (%repl-filesystem-fns)
   :process-fns (%repl-process-fns)
   :redirect-fns (%repl-redirect-fns)
   :terminal-fns nil
   :running *running*
   :last-exit-code *last-exit-code*
   :input-state *input-state*
   :process-registry *proc-registry*))

(defun %sync-repl-shell-context (context code)
  (setf *environment* (nshell.application:shell-context-environment context)
        *aliases* (nshell.application:shell-context-alias-table context)
        *abbreviations* (nshell.application:shell-context-abbreviation-table context)
        *functions* (nshell.application:shell-context-function-table context)
        *running* (nshell.application:shell-context-running context)
        *last-exit-code* code
        *input-state* (nshell.application:shell-context-input-state context)
        *proc-registry* (nshell.application:shell-context-process-registry context))
  code)

(defun %execute-foreground-ast-in-context (ast)
  (let ((context (%make-repl-shell-context)))
    (multiple-value-bind (output code)
        (nshell.application:execute-ast-in-context context ast)
      (%sync-repl-shell-context context (or code 0))
      (when output
        (write-string output))
      (or code 0))))
