;;; REPL completion seed data
(in-package #:nshell.presentation)

(defun seed-repl-completion-knowledge-base (knowledge-base)
  (dolist (spec (nshell.domain.completion:builtin-completion-command-specs)
                knowledge-base)
    (destructuring-bind (command &key flags description) spec
      (nshell.domain.completion:kb-add-command knowledge-base command
                                               :flags flags
                                               :description description))))
