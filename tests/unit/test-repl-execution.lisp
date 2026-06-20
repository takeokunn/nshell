(in-package #:nshell/test)

(in-suite repl-tests)

(test repl-for-loop-expands-in-values
  "Interactive for loops expand variables in the in list before assignment."
  (with-repl-test-state
    (setf nshell.presentation::*environment*
          (nshell.domain.environment:env-set
           nshell.presentation::*environment* "FIRST" "alpha" nil))
    (let ((ast (nshell.domain.parsing::make-for-node
                "item"
                (list "$FIRST" "beta")
                (list (nshell.domain.parsing:make-command-node
                       "echo"
                       (list "$item"))))))
      (multiple-value-bind (output code)
          (call-repl-execute-ast ast)
        (is (= 0 code))
        (is (search "alpha" output))
        (is (search "beta" output))
        (is (not (search "$item" output)))))))

(test repl-execute-parsed-input-records-command-duration
  "Interactive execution records the elapsed runtime for the rendered prompt."
  (with-repl-test-state
    (let ((ast (nshell.domain.parsing:make-command-node "echo" (list "done"))))
      (with-temporary-function
          ('nshell.infrastructure.persistence:append-history-entry
           (lambda (text)
             (declare (ignore text))))
        (with-temporary-function
            ('nshell.presentation::execute-ast
             (lambda (ignored-ast)
               (declare (ignore ignored-ast))
               (sleep 0.05)
               0))
          (nshell.presentation::execute-parsed-input "echo done" ast)))
      (is (integerp nshell.presentation::*last-command-duration-ms*))
      (is (> nshell.presentation::*last-command-duration-ms* 0))
      (is (= 0 nshell.presentation::*last-exit-code*)))))

(test repl-executes-user-function-in-current-context
  "Interactive command execution should invoke user-defined functions."
  (with-repl-test-state
    (setf (gethash "hi" nshell.presentation::*functions*) '("echo from-function"))
    (let ((ast (nshell.domain.parsing:make-command-node "hi" nil)))
      (multiple-value-bind (output code)
          (call-repl-execute-ast ast)
        (is (= 0 code))
        (is (string= (format nil "from-function~%") output))))))

(test repl-pipeline-feeds-builtin-output-to-read
  "Interactive pipelines should feed builtin output into later builtin stages in-process."
  (with-repl-test-state
    (let ((ast (nshell.domain.parsing:make-pipeline-node
                (list (nshell.domain.parsing:make-command-node "echo" (list "piped-value"))
                      (nshell.domain.parsing:make-command-node "read" (list "captured"))))))
      (multiple-value-bind (output code)
          (call-repl-execute-ast ast)
        (is (= 0 code))
        (is (string= "" output))
        (is (string= "piped-value"
                     (nshell.domain.environment:env-get
                      nshell.presentation::*environment*
                      "captured")))))))

(test repl-pipeline-feeds-function-output-to-read
  "Interactive pipelines should pipe function output into builtin stages."
  (with-repl-test-state
    (setf (gethash "produce" nshell.presentation::*functions*) '("echo function-value"))
    (let ((ast (nshell.domain.parsing:make-pipeline-node
                (list (nshell.domain.parsing:make-command-node "produce" nil)
                      (nshell.domain.parsing:make-command-node "read" (list "captured"))))))
      (multiple-value-bind (output code)
          (call-repl-execute-ast ast)
        (is (= 0 code))
        (is (string= "" output))
        (is (string= "function-value"
                     (nshell.domain.environment:env-get
                      nshell.presentation::*environment*
                      "captured")))))))

(test repl-if-node-uses-contextual-pipeline-semantics
  "Interactive control-flow bodies should use the application executor semantics."
  (with-repl-test-state
    (let ((ast (nshell.domain.parsing::make-if-node
                (nshell.domain.parsing:make-command-node "test" (list "ok" "=" "ok"))
                (list (nshell.domain.parsing:make-pipeline-node
                       (list (nshell.domain.parsing:make-command-node "echo" (list "from-if"))
                             (nshell.domain.parsing:make-command-node "read" (list "captured"))))))))
      (multiple-value-bind (output code)
          (call-repl-execute-ast ast)
        (is (= 0 code))
        (is (string= "" output))
        (is (string= "from-if"
                     (nshell.domain.environment:env-get
                      nshell.presentation::*environment*
                      "captured")))))))

(test repl-control-flow-expands-aliases-through-context
  "Aliases should expand inside interactive control-flow execution."
  (with-repl-test-state
    (setf (gethash "say" nshell.presentation::*aliases*) "echo aliased")
    (let ((ast (nshell.domain.parsing::make-if-node
                (nshell.domain.parsing:make-command-node "test" (list "ok" "=" "ok"))
                (list (nshell.domain.parsing:make-command-node "say" (list "value"))))))
      (multiple-value-bind (output code)
          (call-repl-execute-ast ast)
        (is (= 0 code))
        (is (string= (format nil "aliased value~%") output))))))

(test repl-sequence-and-short-circuits-on-failure
  "Interactive `&&` sequences should stop after the first failing command."
  (with-repl-test-state
    (let ((calls nil))
      (with-temporary-function
          ('nshell.presentation::execute-command-node
           (lambda (ast)
             (let ((command (nshell.domain.parsing:command-node-command ast)))
               (push command calls)
               (if (string= command "first")
                   3
                   0))))
          (let ((ast (nshell.domain.parsing::make-sequence-node
                      (list (nshell.domain.parsing:make-command-node "first" nil)
                            (nshell.domain.parsing:make-command-node "second" nil))
                      '(:and))))
            (multiple-value-bind (output code)
                (call-repl-execute-ast ast)
              (declare (ignore output))
              (is (= 3 code)))
            (is (equal '("first") (nreverse calls))))))))

(test repl-sequence-or-short-circuits-on-success
  "Interactive `||` sequences should stop after the first successful command."
  (with-repl-test-state
    (let ((calls nil))
      (with-temporary-function
          ('nshell.presentation::execute-command-node
           (lambda (ast)
             (let ((command (nshell.domain.parsing:command-node-command ast)))
               (push command calls)
               (if (string= command "first")
                   0
                   5))))
          (let ((ast (nshell.domain.parsing::make-sequence-node
                      (list (nshell.domain.parsing:make-command-node "first" nil)
                            (nshell.domain.parsing:make-command-node "second" nil))
                      '(:or))))
            (multiple-value-bind (output code)
                (call-repl-execute-ast ast)
              (declare (ignore output))
              (is (= 0 code)))
            (is (equal '("first") (nreverse calls))))))))
