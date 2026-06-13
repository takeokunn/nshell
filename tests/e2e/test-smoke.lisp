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
  "Verify pipeline infrastructure works via ACL"
  (let ((exit-code (nshell.infrastructure.acl:run-external "echo" '("test"))))
    (is (= 0 exit-code))))
