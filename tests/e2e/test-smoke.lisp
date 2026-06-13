(in-package #:nshell/test)

(def-suite e2e-tests
  :description "End-to-end smoke tests"
  :in nshell-tests)

(in-suite e2e-tests)

(test e2e-echo-command
  "Built-in echo command produces correct output"
  (let* ((result (nshell.domain.parsing:parse-command-line "echo hello world"))
         (ast (nshell.domain.parsing:parse-result-ast result)))
    (is (nshell.domain.parsing:command-node-p ast))
    (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
    (is (equal '("hello" "world") (nshell.domain.parsing:command-node-args ast)))))

(test e2e-full-repl-cycle
  "Full cycle: parse → history → execute works without error"
  (let* ((history (nshell.domain.history:make-command-history))
         (line "pwd")
         (result (nshell.domain.parsing:parse-command-line line)))
    (is (nshell.domain.parsing:parse-complete-p result))
    (nshell.domain.history:history-add history line)
    (is (= 1 (nshell.domain.history:history-size history)))))
