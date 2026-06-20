(in-package #:nshell/test)

(in-suite builtin-tests)

(defun %source-sequence-call-order (separator first-code second-code)
  (let ((context (make-test-builtins-context))
        (calls nil))
    (with-temporary-function
        ('nshell.application::execute-ast-in-context
         (lambda (_context ast)
           (declare (ignore _context))
           (let ((command (nshell.domain.parsing:command-node-command ast)))
             (push command calls)
             (values nil (if (string= command "first")
                             first-code
                             second-code)))))
      (let ((ast (nshell.domain.parsing::make-sequence-node
                  (list (nshell.domain.parsing:make-command-node "first" nil)
                        (nshell.domain.parsing:make-command-node "second" nil))
                  (list separator))))
        (multiple-value-bind (output code)
            (nshell.application::%execute-sequence-node-in-context context ast)
          (values output code (nreverse calls)))))))

(test source-sequence-and-short-circuits-on-failure
  "source stops a sequence after a failing && command."
  (multiple-value-bind (output code calls)
      (%source-sequence-call-order :and 1 0)
    (is (string= "" output))
    (is (= 1 code))
    (is (equal '("first") calls))))

(test source-sequence-and-continues-on-success
  "source continues past a successful && command."
  (multiple-value-bind (output code calls)
      (%source-sequence-call-order :and 0 0)
    (is (string= "" output))
    (is (= 0 code))
    (is (equal '("first" "second") calls))))

(test source-sequence-or-short-circuits-on-success
  "source stops a sequence after a successful || command."
  (multiple-value-bind (output code calls)
      (%source-sequence-call-order :or 0 0)
    (is (string= "" output))
    (is (= 0 code))
    (is (equal '("first") calls))))

(test source-sequence-or-continues-on-failure
  "source continues past a failing || command."
  (multiple-value-bind (output code calls)
      (%source-sequence-call-order :or 1 0)
    (is (string= "" output))
    (is (= 0 code))
    (is (equal '("first" "second") calls))))

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

(test function-receives-arguments-via-argv
  "A called function sees its arguments through $argv (forwarded as words)."
  (with-builtins-source (output code context
                                 '("function greet"
                                   "echo hi $argv"
                                   "end"
                                   "greet world and friends"))
    (is (= 0 code))
    (is (string= (format nil "hi world and friends~%") output))))

(test function-argv-indexing-selects-single-argument
  "$argv[N] selects the Nth (1-based) argument inside a function body."
  (with-builtins-source (output code context
                                 '("function pick"
                                   "echo got $argv[2]"
                                   "end"
                                   "pick one two three"))
    (is (= 0 code))
    (is (string= (format nil "got two~%") output))))

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

(test source-function-body-supports-nested-begin
  "source keeps nested begin/end blocks inside function definitions."
  (with-builtins-source (output code context
                                 '("function wrap"
                                   "begin"
                                   "echo function-begin"
                                   "end"
                                   "echo after-begin"
                                   "end"
                                   "wrap"))
    (is (= 0 code))
    (is (string= (format nil "function-begin~%after-begin~%") output))
    (is (equal '("begin"
                 "echo function-begin"
                 "end"
                 "echo after-begin")
               (gethash "wrap"
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

(test source-pipeline-uses-source-strategy-for-external-pipelines
  "source keeps external pipelines on the source execution path when strategy is :cps."
  (skip-in-sandbox "executes /bin/echo and /bin/cat"
  (let ((context (make-test-builtins-context)))
    (setf (nshell.application:shell-context-execution-strategy context) :cps)
    (with-temporary-function
        ('nshell.infrastructure.acl:spawn-pipeline
         (lambda (&rest _args)
           (declare (ignore _args))
           (error "spawn-pipeline should not run for :cps")))
      (with-called-source (output code context
                                  '("/bin/echo cps-strategy | /bin/cat"))
        (is (= 0 code))
        (is (string= (format nil "cps-strategy~%") output)))))))

(test source-pipeline-uses-os-pipes-strategy-for-external-pipelines
  "source dispatches external pipelines to spawn-pipeline when strategy is :os-pipes."
  (let ((context (make-test-builtins-context))
        (called nil)
        (command-count nil)
        (captured-redirects nil))
    (setf (nshell.application:shell-context-execution-strategy context) :os-pipes)
    (with-temporary-function
        ('nshell.infrastructure.acl:spawn-pipeline
         (lambda (commands &key redirects)
           (setf called t
                 command-count (length commands)
                 captured-redirects redirects)
           (format t "spawned-path~%")
           37))
      (with-called-source (output code context
                                  '("/bin/echo os-pipes-strategy | /bin/cat"))
        (is (not (null called)))
        (is (= 2 command-count))
        (is (listp captured-redirects))
        (is (= 37 code))
        (is (string= (format nil "spawned-path~%") output))))))

(test source-pipeline-keeps-internal-commands-on-source-path-under-os-pipes
  "source still executes pipelines with internal commands through the source path even when strategy is :os-pipes."
  (let ((context (make-test-builtins-context)))
    (setf (nshell.application:shell-context-execution-strategy context) :os-pipes)
    (with-temporary-function
        ('nshell.infrastructure.acl:spawn-pipeline
         (lambda (&rest _args)
           (declare (ignore _args))
           (error "spawn-pipeline should not run for internal commands")))
      (with-called-source (output code context
                                  '("echo internal-value | read captured"))
        (is (= 0 code))
        (is (string= "" output))
        (is (string= "internal-value"
                     (nshell.domain.environment:env-get
                      (nshell.application:shell-context-environment context)
                      "captured")))))))

(test pbt-source-pipeline-keeps-external-only-pipelines-on-source-path-under-cps
  "Generated external-only pipelines stay on the source path when strategy is :cps."
  (skip-in-sandbox "executes /bin/echo and /bin/cat"
  (check-property (:trials 50)
      ((payload (gen-shell-word :min-length 1 :max-length 8)
                #'shrink-prompt-text))
    (let ((context (make-test-builtins-context))
          (spawned nil))
      (setf (nshell.application:shell-context-execution-strategy context) :cps)
      (with-temporary-function
          ('nshell.infrastructure.acl:spawn-pipeline
           (lambda (&rest _args)
             (declare (ignore _args))
             (setf spawned t)
             (error "spawn-pipeline should not run for :cps")))
        (with-called-source (output code context
                                (list (format nil "/bin/echo ~a | /bin/cat" payload)))
          (is (not spawned))
          (is (= 0 code))
          (is (string= (format nil "~a~%" payload) output))))))))

(test pbt-source-pipeline-routes-external-only-pipelines-to-spawn-pipeline-under-os-pipes
  "Generated external-only pipelines route through spawn-pipeline when strategy is :os-pipes."
  (check-property (:trials 50)
      ((payload (gen-shell-word :min-length 1 :max-length 8)
                #'shrink-prompt-text))
    (let ((context (make-test-builtins-context))
          (called nil))
      (setf (nshell.application:shell-context-execution-strategy context) :os-pipes)
      (with-temporary-function
          ('nshell.infrastructure.acl:spawn-pipeline
           (lambda (commands &key redirects)
             (setf called t)
             (is (= 2 (length commands)))
             (is (listp redirects))
             (format t "spawned-path~%")
             37))
        (with-called-source (output code context
                                (list (format nil "/bin/echo ~a | /bin/cat" payload)))
          (is (not (null called)))
          (is (= 37 code))
          (is (search "spawned-path" output)))))))
