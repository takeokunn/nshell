(in-package #:nshell/test)

(def-suite repl-tests
  :description "REPL presentation boundary tests"
  :in nshell-tests)

(in-suite repl-tests)

(test repl-batch-returns-last-exit-code
  "Batch execution should return the last command status for process exit."
  (with-repl-test-state
    (with-temporary-function
        ('nshell.presentation::execute-ast
         (lambda (ast)
           (declare (ignore ast))
           7))
      (with-input-from-string (*standard-input* (format nil "echo hello~%"))
        (let ((code (nshell.presentation::run-repl-batch)))
          (is (= 7 code))
          (is (= 7 nshell.presentation::*last-exit-code*)))))))
