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
      (let ((output (with-output-to-string (*standard-output*)
                      (is (= 0 (nshell.presentation::execute-ast ast))))))
        (is (search "alpha" output))
        (is (search "beta" output))
        (is (not (search "$item" output)))))))

(test repl-executes-user-function-in-current-context
  "Interactive command execution should invoke user-defined functions."
  (with-repl-test-state
    (setf (gethash "hi" nshell.presentation::*functions*) '("echo from-function"))
    (let* ((code nil)
           (ast (nshell.domain.parsing:make-command-node "hi" nil))
           (output
             (with-output-to-string (*standard-output*)
               (setf code (nshell.presentation::execute-ast ast)))))
      (is (= 0 code))
      (is (string= (format nil "from-function~%") output)))))

(test repl-pipeline-feeds-builtin-output-to-read
  "Interactive pipelines should feed builtin output into later builtin stages in-process."
  (with-repl-test-state
    (let* ((code nil)
           (ast
             (nshell.domain.parsing:make-pipeline-node
              (list (nshell.domain.parsing:make-command-node "echo" (list "piped-value"))
                    (nshell.domain.parsing:make-command-node "read" (list "captured")))))
           (output
             (with-output-to-string (*standard-output*)
               (setf code (nshell.presentation::execute-ast ast)))))
      (is (= 0 code))
      (is (string= "" output))
      (is (string= "piped-value"
                   (nshell.domain.environment:env-get
                    nshell.presentation::*environment*
                    "captured"))))))

(test repl-pipeline-feeds-function-output-to-read
  "Interactive pipelines should pipe function output into builtin stages."
  (with-repl-test-state
    (setf (gethash "produce" nshell.presentation::*functions*) '("echo function-value"))
    (let* ((code nil)
           (ast
             (nshell.domain.parsing:make-pipeline-node
              (list (nshell.domain.parsing:make-command-node "produce" nil)
                    (nshell.domain.parsing:make-command-node "read" (list "captured")))))
           (output
             (with-output-to-string (*standard-output*)
               (setf code (nshell.presentation::execute-ast ast)))))
      (is (= 0 code))
      (is (string= "" output))
      (is (string= "function-value"
                   (nshell.domain.environment:env-get
                    nshell.presentation::*environment*
                    "captured"))))))

(test repl-if-node-uses-contextual-pipeline-semantics
  "Interactive control-flow bodies should use the application executor semantics."
  (with-repl-test-state
    (let* ((code nil)
           (ast
             (nshell.domain.parsing::make-if-node
              (nshell.domain.parsing:make-command-node "test" (list "ok" "=" "ok"))
              (list (nshell.domain.parsing:make-pipeline-node
                     (list (nshell.domain.parsing:make-command-node "echo" (list "from-if"))
                           (nshell.domain.parsing:make-command-node "read" (list "captured")))))))
           (output
             (with-output-to-string (*standard-output*)
               (setf code (nshell.presentation::execute-ast ast)))))
      (is (= 0 code))
      (is (string= "" output))
      (is (string= "from-if"
                   (nshell.domain.environment:env-get
                    nshell.presentation::*environment*
                    "captured"))))))

(test repl-control-flow-expands-aliases-through-context
  "Aliases should expand inside interactive control-flow execution."
  (with-repl-test-state
    (setf (gethash "say" nshell.presentation::*aliases*) "echo aliased")
    (let* ((code nil)
           (ast
             (nshell.domain.parsing::make-if-node
              (nshell.domain.parsing:make-command-node "test" (list "ok" "=" "ok"))
              (list (nshell.domain.parsing:make-command-node "say" (list "value")))))
           (output
             (with-output-to-string (*standard-output*)
               (setf code (nshell.presentation::execute-ast ast)))))
      (is (= 0 code))
      (is (string= (format nil "aliased value~%") output)))))
