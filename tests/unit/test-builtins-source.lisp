(in-package #:nshell/test)

(in-suite builtin-tests)

(test source-function-body-supports-nested-control-flow
  "source keeps nested blocks inside function definitions instead of closing at the first end."
  (with-builtins-source (output code context
                                 '("function nested"
                                   "if true"
                                   "echo function-inner"
                                   "else"
                                   "echo function-else"
                                   "end"
                                   "echo function-after"
                                   "end"
                                   "nested"
                                   "echo script-after"))
    (is (= 0 code))
    (is (string= (format nil "function-inner~%function-after~%script-after~%")
                 output))
    (is (equal '("if true"
                 "echo function-inner"
                 "else"
                 "echo function-else"
                 "end"
                 "echo function-after")
               (gethash "nested"
                        (nshell.application:shell-context-function-table
                         context))))))

(test source-function-definition-supports-inline-body
  "source registers functions defined on a single line with an inline body."
  (with-builtins-source (output code context
                                 '("function foo; echo hi; end"
                                   "foo"))
    (is (= 0 code))
    (is (string= (format nil "hi~%hi~%") output))
    (is (equal '("echo hi")
               (gethash "foo"
                        (nshell.application:shell-context-function-table
                         context))))))

(test source-function-definition-preserves-trailing-inline-commands
  "source keeps commands that follow an inline function definition on the same line."
  (with-builtins-source (output code context
                                 '("function foo; echo hi; end; foo"))
    (is (= 0 code))
    (is (string= (format nil "hi~%hi~%") output))
    (is (equal '("echo hi")
               (gethash "foo"
                        (nshell.application:shell-context-function-table
                         context))))))

(test source-switch-case-executes-matching-clause
  "source executes fish-style switch/case blocks."
  (with-builtins-source (output code context
                                 '("switch chocolate"
                                   "case vanilla"
                                   "echo plain"
                                   "case chocolate strawberry"
                                   "echo sweet"
                                   "case '*'"
                                   "echo default"
                                   "end"))
    (is (= 0 code))
    (is (string= (format nil "sweet~%") output))))

(test source-switch-case-supports-default-pattern
  "source executes the default switch/case clause when no exact pattern matches."
  (with-builtins-source (output code context
                                 '("switch mint"
                                   "case vanilla"
                                   "echo plain"
                                   "case chocolate strawberry"
                                   "echo sweet"
                                   "case '*'"
                                   "echo default"
                                   "end"))
    (is (= 0 code))
    (is (string= (format nil "default~%") output))))

(test source-if-supports-not-command-modifier
  "source lets fish-style if conditions invert command status with not."
  (with-builtins-source (output code context
                                 '("if not test -f /tmp/file.txt"
                                   "echo missing"
                                   "else"
                                   "echo exists"
                                   "end"
                                   "if not test -f /tmp/missing"
                                   "echo absent"
                                   "else"
                                   "echo present"
                                   "end"))
    (is (= 0 code))
    (is (string= (format nil "exists~%absent~%") output))))

(test source-command-substitution-expands-function-output
  "source expands fish-style command substitutions and splits output on newlines."
  (with-builtins-source (output code context
                                 '("function produce"
                                   "echo alpha"
                                   "echo beta"
                                   "end"
                                   "echo before (produce) after"))
    (is (= 0 code))
    (is (string= (format nil "before alpha beta after~%") output))))

(test source-command-substitution-expands-inside-double-quotes
  "source supports embedded command substitutions inside double-quoted words."
  (with-builtins-source (output code context
                                 '("echo \"file-(echo main).lisp\""))
    (is (= 0 code))
    (is (string= (format nil "file-main.lisp~%") output))))

(test source-command-substitution-expands-external-output
  "source expands external command substitution output when capture is available."
  (let ((context (make-test-builtins-context
                  :external-capture-runner
                  (lambda (command args)
                    (is (string= "capture-values" command))
                    (is (null args))
                    (values (format nil "red~%blue~%") 0)))))
    (with-called-source (output code context
                                '("echo before (capture-values) after"))
      (is (= 0 code))
      (is (string= (format nil "before red blue after~%") output)))))

(test source-command-substitution-keeps-single-quoted-words-literal
  "source does not expand command substitutions in single-quoted words."
  (with-builtins-source (output code context
                                 '("echo '(echo nope)'"))
    (is (= 0 code))
    (is (string= (format nil "(echo nope)~%") output))))

(test source-for-loop-expands-command-substitution-values
  "source lets fish-style for loops iterate over command substitution lines."
  (with-builtins-source (output code context
                                 '("function values"
                                   "echo one"
                                   "echo two"
                                   "end"
                                   "for item in (values)"
                                   "echo item=$item"
                                   "end"))
    (is (= 0 code))
    (is (string= (format nil "item=one~%item=two~%") output))))

(test source-function-body-supports-nested-switch
  "source keeps nested switch/case blocks inside function definitions."
  (with-builtins-source (output code context
                                 '("function choose"
                                   "switch chocolate"
                                   "case chocolate"
                                   "echo function-sweet"
                                   "end"
                                   "echo after-switch"
                                   "end"
                                   "choose"))
    (is (= 0 code))
    (is (string= (format nil "function-sweet~%after-switch~%") output))
    (is (equal '("switch chocolate"
                 "case chocolate"
                 "echo function-sweet"
                 "end"
                 "echo after-switch")
               (gethash "choose"
                        (nshell.application:shell-context-function-table
                         context))))))

(test source-pipeline-feeds-builtin-output-to-read
  "source executes builtin pipeline stages in the current shell context."
  (with-builtins-source (output code context
                                 '("echo piped-value | read captured"))
    (is (string= "" output))
    (is (= 0 code))
    (is (string= "piped-value"
                 (nshell.domain.environment:env-get
                  (nshell.application:shell-context-environment context)
                  "captured")))))

(test source-pipeline-feeds-function-output-to-read
  "source lets fish-style functions participate in pipelines."
  (with-builtins-source (output code context
                                 '("function produce"
                                   "echo function-value"
                                   "end"
                                   "produce | read captured"))
    (is (string= "" output))
    (is (= 0 code))
    (is (string= "function-value"
                 (nshell.domain.environment:env-get
                  (nshell.application:shell-context-environment context)
                  "captured")))))

(test source-pipeline-redirects-builtin-output
  "source supports redirection on builtin pipeline stages."
  (with-builtins-source-tree (context root source :prefix "nshell-test-source-redirect")
    (let ((target (merge-pathnames "out.txt" root)))
      (write-test-lines source
                        (list (format nil "echo redirected > ~a"
                                      (namestring target))))
      (multiple-value-bind (output code)
          (call-source-file context source)
        (is (string= "" output))
        (is (= 0 code))
        (is (string= "redirected" (read-test-file-line target)))))))

(test source-pipeline-redirects-function-output
  "source redirects fish-style function output from pipeline stages."
  (with-builtins-source-tree (context root source :prefix "nshell-test-source-function-redirect")
    (let ((target (merge-pathnames "function.txt" root)))
      (write-test-lines source
                        (list "function produce"
                              "echo function-redirected"
                              "end"
                              (format nil "produce > ~a" (namestring target))))
      (multiple-value-bind (output code)
          (call-source-file context source)
        (is (string= "" output))
        (is (= 0 code))
        (is (string= "function-redirected"
                     (read-test-file-line target)))))))

(test source-pipeline-input-redirect-overrides-pipe-input
  "source applies input redirects on builtin pipeline stages."
  (with-builtins-source-tree (context root source :prefix "nshell-test-source-input-redirect")
    (let ((input (merge-pathnames "input.txt" root)))
      (write-test-lines input '("from-file"))
      (write-test-lines source
                        (list (format nil "echo from-pipe | read captured < ~a"
                                      (namestring input))))
      (multiple-value-bind (output code)
          (call-source-file context source)
        (is (string= "" output))
        (is (= 0 code))
        (is (string= "from-file"
                     (nshell.domain.environment:env-get
                      (nshell.application:shell-context-environment context)
                      "captured")))))))
