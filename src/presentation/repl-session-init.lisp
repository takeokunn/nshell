;;; REPL state initialization
(in-package #:nshell.presentation)

(defun load-history-into-repl ()
  (let ((saved (nshell.infrastructure.persistence:load-history-file)))
    (dolist (entry (reverse saved))
      (nshell.domain.history:history-add *history* entry))))

(defun initialize-repl-state ()
  (setf *running* t
        *last-exit-code* 0
        *last-command-duration-ms* nil
        *history* (nshell.domain.history:make-command-history)
        *config* (nshell.domain.configuration:default-config)
        *kb* (nshell.domain.completion:make-knowledge-base)
        *input-state* (make-repl-input-state)
        *completion-rendered-lines* 0
        *prompt-rendered-lines* 0
        *prompt-rendered-cursor-row* 0
        *environment* (nshell.domain.environment:inject-os-environment
                       (nshell.domain.environment:make-default-environment))
        *aliases* (make-hash-table :test #'equal)
        *abbreviations* (make-hash-table :test #'equal)
        *functions* (make-hash-table :test #'equal)
        *function-sources* (make-hash-table :test #'equal)
        *proc-registry* (make-hash-table :test #'eql))
  (install-expansion-filesystem)
  (configure-completion-filesystem)
  (load-history-into-repl)
  (seed-repl-completion-knowledge-base *kb*))
