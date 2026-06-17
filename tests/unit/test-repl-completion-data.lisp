(in-package #:nshell/test)

(in-suite repl-tests)

(test repl-command-specs-are-unique
  "The REPL completion seed data should not define the same command twice."
  (let* ((commands (mapcar #'first nshell.presentation::+repl-completion-command-specs+))
         (unique-commands (remove-duplicates commands :test #'string=)))
    (is (= (length commands) (length unique-commands)))))

(test repl-command-data-seeds-completion-knowledge-base
  "REPL completion command data is converted into command and flag facts."
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.presentation::seed-repl-completion-knowledge-base kb)
    (let ((command-texts (repl-completion-texts
                          (nshell.domain.completion:complete kb "a"))))
      (is (member "abbr" command-texts :test #'string=))
      (is (member "alias" command-texts :test #'string=)))
    (is (member "-q"
                (repl-completion-texts
                 (nshell.domain.completion:complete kb "abbr -"))
                :test #'string=))
    (is (member "--show"
                (repl-completion-texts
                 (nshell.domain.completion:complete kb "abbr --"))
                :test #'string=))
    (is (member "-x"
                (repl-completion-texts
                 (nshell.domain.completion:complete kb "set -"))
                :test #'string=))
    (is (member "--query"
                (repl-completion-texts
                 (nshell.domain.completion:complete kb "set --"))
                :test #'string=))
    (is (member "replace"
                (repl-completion-texts
                 (nshell.domain.completion:complete kb "string r"))
                :test #'string=))
    (is (member "--all"
                (repl-completion-texts
                 (nshell.domain.completion:complete kb "string --"))
                :test #'string=))))
