(in-package #:nshell/test)
(def-suite e2e-tests :description "E2E smoke tests" :in nshell-tests)
(in-suite e2e-tests)
(test e2e-echo-command
  (let* ((result (nshell.domain.parsing:parse-command-line "echo hello world"))
         (ast (nshell.domain.parsing:parse-result-ast result)))
    (is (nshell.domain.parsing:command-node-p ast))
    (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
    (is (equal '("hello" "world") (nshell.domain.parsing:command-node-args ast)))))
(test e2e-full-repl-cycle
  (let* ((history (nshell.domain.history:make-command-history))
         (line "pwd")
         (result (nshell.domain.parsing:parse-command-line line)))
    (is (nshell.domain.parsing:parse-complete-p result))
    (nshell.domain.history:history-add history line)
    (is (= 1 (nshell.domain.history:history-size history)))))
(test e2e-pipeline-smoke
  "Verify pipeline execution via spawn-pipeline"
  (let* ((cmd1 (nshell.domain.parsing:make-command-node "echo" '("hello")))
         (pipe (nshell.domain.parsing:make-pipeline-node (list cmd1)))
         (exit (nshell.infrastructure.acl:spawn-pipeline
                (nshell.domain.parsing:pipeline-node-commands pipe))))
    (is (= 0 exit))))
(test e2e-external-command
  "External command execution returns correct exit code"
  (is (= 0 (nshell.infrastructure.acl:run-external "true" '())))
  (is (not (= 0 (nshell.infrastructure.acl:run-external "false" '())))))
