(in-package #:nshell/test)
(def-suite e2e-tests :description "E2E smoke tests" :in nshell-tests)
(in-suite e2e-tests)

(defun %nshell-main-form (arguments)
  (format nil
          "(progn
             (asdf:load-system :nshell)
             (let ((sb-ext:*posix-argv* (list ~{~S~^ ~})))
               (funcall (symbol-function (find-symbol \"MAIN\" \"NSHELL\")))))"
          (cons "nshell" arguments)))

(defun %run-nshell-main (arguments)
  (let ((root (asdf:system-source-directory :nshell)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program
         (list (current-sbcl-executable)
               "--noinform"
               "--eval" "(require :asdf)"
               "--eval" "(push (truename \"./\") asdf:*central-registry*)"
               "--eval" (%nshell-main-form arguments))
         :directory root
         :output :string
         :error-output :string
         :ignore-error-status t)
      (values stdout stderr exit-code))))

(defun %assert-nshell-main-result (arguments expected-output expected-code
                                 &key expected-error)
  (multiple-value-bind (stdout stderr exit-code)
      (%run-nshell-main arguments)
    (is (= expected-code exit-code))
    (when expected-output
      (is (search expected-output stdout)
          "stdout should contain ~S, got ~S"
          expected-output stdout))
    (when expected-error
      (is (search expected-error stderr)
          "stderr should contain ~S, got ~S"
          expected-error stderr))
    (unless expected-error
      (is (string= "" stderr)))
    (values stdout stderr exit-code)))

(test e2e-echo-command
  (with-complete-command-line (result ast "echo hello world")
    (is (nshell.domain.parsing:command-node-p ast))
    (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
    (is (equal '("hello" "world") (nshell.domain.parsing:command-node-args ast)))))
(test e2e-full-repl-cycle
  (let* ((history (nshell.domain.history:make-command-history))
         (line "pwd"))
    (with-parsed-command-line (result line)
      (is (nshell.domain.parsing:parse-complete-p result)))
    (nshell.domain.history:history-add history line)
    (is (= 1 (nshell.domain.history:history-size history)))))

(test e2e-main-help-exits-cleanly
  "The entry point prints usage text and exits successfully for --help."
  (%assert-nshell-main-result '("--help")
                              "Usage: nshell [--help] [--version] [-c COMMAND]"
                              0))

(test e2e-main-version-exits-cleanly
  "The entry point prints a version banner and exits successfully for --version."
  (%assert-nshell-main-result '("--version")
                              "nshell v"
                              0))

(test e2e-main-invalid-args-report-usage
  "The entry point rejects unsupported option flags with a usage message."
  (%assert-nshell-main-result '("--unknown")
                              nil
                              1
                              :expected-error "Usage: nshell [--help] [--version] [-c COMMAND]"))

(test e2e-run-script-file-executes-multiline-blocks
  "Running a script file executes multiline blocks and exposes $argv."
  (with-temporary-output-file (path :prefix "nshell-script")
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string (format nil "function show~%echo hi $argv[1]~%end~%for i in (seq 1 2)~%echo n=$i~%end~%show $argv~%")
                    out))
    (let ((output (capture-standard-output
                    (nshell.presentation::run-repl-script path '("World")))))
      (is (search "n=1" output))
      (is (search "n=2" output))
      (is (search "hi World" output)))))

(test e2e-main-command-executes-once
  "The entry point executes a single batch command with -c."
  (%assert-nshell-main-result '("-c" "echo hello")
                              "hello"
                              0))

(test e2e-main-type-command-executes-cleanly
  "The entry point executes type through the batch command path."
  (%assert-nshell-main-result '("-c" "type echo")
                              "echo is a shell builtin"
                              0))

(test e2e-abbreviation-expands-on-enter-before-execution
  (with-repl-test-state
    (setf (gethash "say" nshell.presentation::*abbreviations*) "echo hello")
    (setf nshell.presentation::*input-state*
          (nshell.presentation::make-repl-input-state :buffer "say"))
    (multiple-value-bind (state output)
        (reduce-once nshell.presentation::*input-state* :enter)
      (setf nshell.presentation::*input-state* state)
      (is-input-state state :buffer "echo hello" :cursor-pos 10)
      (is (eq :execute output))
      (let ((rendered (capture-process-output-event output)))
        (is (search "hello" rendered))
        (is (= 0 nshell.presentation::*last-exit-code*))
        (is (string= ""
                     (nshell.presentation:input-state-buffer
                      nshell.presentation::*input-state*)))))))

(test e2e-command-position-abbreviation-expands-only-at-command-position
  (with-repl-test-state
    (setf (gethash "gco" nshell.presentation::*abbreviations*)
          (nshell.domain.abbreviation:make-abbreviation
           :expansion "echo command"
           :position :command))
    (setf nshell.presentation::*input-state*
          (nshell.presentation::make-repl-input-state :buffer "echo gco"))
    (multiple-value-bind (state output)
        (reduce-once nshell.presentation::*input-state* :enter)
      (is-input-state state :buffer "echo gco" :cursor-pos 8)
      (is (eq :execute output)))
    (setf nshell.presentation::*input-state*
          (nshell.presentation::make-repl-input-state :buffer "gco"))
    (multiple-value-bind (state output)
        (reduce-once nshell.presentation::*input-state* :enter)
      (is-input-state state :buffer "echo command" :cursor-pos 12)
      (is (eq :execute output)))))

(test e2e-meta-s-input-cycle
  (let* ((events (read-key-events-from-string
                  (concatenate 'string "apt update" (esc-sequence "s"))))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "sudo apt update" line))
    (with-complete-command-line (result ast line)
      (is (string= "sudo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("apt" "update")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-t-input-cycle
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "gti status" 'list)
                                  (make-list 8 :initial-element (code-char 2))
                                  (list (code-char 20)))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status" line))
    (with-complete-command-line (result ast line)
      (is (string= "git" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("status")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-alt-t-input-cycle
  (let* ((events (read-key-events-from-string
                  (concatenate 'string "echo world hello" (esc-sequence "t"))))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo hello world" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hello" "world")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-alt-u-input-cycle
  (let* ((events (read-key-events-from-string
                  (concatenate 'string "echo hello" (esc-sequence "u"))))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo HELLO" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("HELLO")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-bracketed-paste-normalizes-newlines-and-undos-once
  (let* ((raw-paste (format nil "echo one~C~Cecho two~C"
                            #\Return #\Newline #\Return))
         (expected (format nil "echo one~%echo two~%"))
         (events (read-key-events-from-string
                  (concatenate 'string
                               (esc-sequence "[200~")
                               raw-paste
                               (esc-sequence "[201~"))))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events)))
    (is-input-state state
                    :buffer expected
                    :cursor-pos (length expected))
    (multiple-value-bind (undone output)
        (reduce-once state :ctrl-underscore)
      (is-input-state undone :buffer "" :cursor-pos 0)
      (is (eq :suggest-update output)))))

(test e2e-alt-t-preserves-quoted-word-cycle
  (let* ((events (read-key-events-from-string
                  (concatenate 'string
                               "echo tail \"hello world\""
                               (esc-sequence "t"))))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo \"hello world\" tail" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hello world" "tail")
                 (nshell.domain.parsing:command-node-arg-values ast))))))

(test e2e-alt-d-preserves-quoted-word-cycle
  (let* ((events (read-key-events-from-string (esc-sequence "d")))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "echo \"hello world\" tail"
                  :cursor-pos 4)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo tail" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("tail")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-k-replaces-line-suffix
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "echo hello world" 'list)
                                  (make-list 5 :initial-element (code-char 2))
                                  (list (code-char 11))
                                  (coerce "shell" 'list))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo hello shell" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hello" "shell")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-u-yank-restores-killed-line
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "echo hello world" 'list)
                                  (list (code-char 21)
                                        (code-char 25)))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo hello world" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hello" "world")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-w-yank-restores-escaped-word
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "echo hello\\ world" 'list)
                                  (list (code-char 23)
                                        (code-char 25)))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo hello\\ world" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hello world")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-g-cancels-completion-session
  (let* ((events (read-key-events-from-string (string (code-char 7))))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "g"
                  :cursor-pos 1
                  :completion-index 0
                  :completion-base-buffer "g"
                  :completion-base-cursor 1
                  :last-candidates '("git" "grep")
                  :suggestion "it")
                 events)))
    (is-input-state state
                    :buffer "g"
                    :cursor-pos 1
                    :completion-index -1
                    :completion-base-buffer nil
                    :completion-base-cursor nil
                    :last-candidates nil
                    :suggestion nil)))

(test e2e-end-accepts-autosuggestion-tail
  (let* ((events (read-key-events-from-string (esc-sequence "[F")))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "git"
                  :cursor-pos 3
                  :suggestion " status")
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status" line))
    (with-complete-command-line (result ast line)
      (is (string= "git" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("status")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-e-accepts-autosuggestion-tail
  (let* ((events (read-key-events-from-string (string (code-char 5))))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "git"
                  :cursor-pos 3
                  :suggestion " status")
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status" line))
    (with-complete-command-line (result ast line)
      (is (string= "git" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("status")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-right-and-ctrl-f-accept-autosuggestion-tail
  "Decoded Right and Ctrl-F both accept the complete autosuggestion tail at line end."
  (let* ((right-events (read-key-events-from-string (esc-sequence "[C")))
         (ctrl-f-events (read-key-events-from-string (string (code-char 6))))
         (right-state (apply-key-events-to-input-state
                       (input-state
                        :buffer "git"
                        :cursor-pos 3
                        :suggestion " status")
                       right-events))
         (ctrl-f-state (apply-key-events-to-input-state
                        (input-state
                        :buffer "git"
                        :cursor-pos 3
                        :suggestion " status")
                        ctrl-f-events)))
    (is (string= (nshell.presentation:input-state-buffer right-state)
                 (nshell.presentation:input-state-buffer ctrl-f-state)))
    (is (string= "git status"
                 (nshell.presentation:input-state-buffer right-state)))
    (is (= (nshell.presentation:input-state-cursor-pos right-state)
           (nshell.presentation:input-state-cursor-pos ctrl-f-state)))
    (is (null (nshell.presentation:input-state-suggestion right-state)))
    (is (null (nshell.presentation:input-state-suggestion ctrl-f-state)))))

(test e2e-alt-right-accepts-autosuggestion-operator-then-command
  (let* ((events (read-key-events-from-string
                  (concatenate 'string (esc-sequence "f") (esc-sequence "f"))))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "git status"
                  :cursor-pos 10
                  :suggestion " | grep modified")
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status | grep" line))
    (is (string= " modified"
                 (nshell.presentation:input-state-suggestion state)))
    (with-complete-command-line (result ast line)
      (let ((commands (nshell.domain.parsing:pipeline-node-commands ast)))
        (is (= 2 (length commands)))
        (is (string= "git"
                     (nshell.domain.parsing:command-node-command (first commands))))
        (is (equal '("status")
                   (nshell.domain.parsing:command-node-args (first commands))))
        (is (string= "grep"
                     (nshell.domain.parsing:command-node-command (second commands))))))))

(test e2e-ctrl-right-accepts-autosuggestion-word
  (let* ((events (read-key-events-from-string (esc-sequence "[1;5C")))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "git"
                  :cursor-pos 3
                  :suggestion " status --short")
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status" line))
    (is (string= " --short"
                 (nshell.presentation:input-state-suggestion state)))
    (with-complete-command-line (result ast line)
      (is (string= "git" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("status")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-alt-right-accepts-attached-redirection-target
  (let* ((events (read-key-events-from-string (esc-sequence "f")))
         (state (apply-key-events-to-input-state
                 (input-state
                  :buffer "echo hi"
                  :cursor-pos 7
                  :suggestion " >out.txt && cat out.txt")
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo hi >out.txt" line))
    (is (string= " && cat out.txt"
                 (nshell.presentation:input-state-suggestion state)))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hi" (">" . nil) "out.txt")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-control-h-backspace-input-cycle
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "git statusx" 'list)
                                  (list (code-char 8)))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status" line))
    (with-complete-command-line (result ast line)
      (is (string= "git" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("status")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-d-deletes-character-under-cursor
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "echo hxello" 'list)
                                  (make-list 5 :initial-element (code-char 2))
                                  (list (code-char 4)))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo hello" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("hello")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-ctrl-d-on-empty-input-requests-quit
  (let* ((events (read-key-events-from-string (string (code-char 4))))
         (event (first events)))
    (multiple-value-bind (state output)
        (nshell.presentation:reduce-input-state (input-state) event)
      (is-input-state state
                      :buffer ""
                      :cursor-pos 0)
      (is (eq :quit output)))))

(test e2e-ctrl-l-clears-screen-without-losing-editing-session
  (with-repl-test-state
    (let ((state (input-state
                  :buffer "git"
                  :cursor-pos 3
                  :completion-index 0
                  :completion-base-buffer "git"
                  :completion-base-cursor 3
                  :last-candidates '("git" "grep")
                  :suggestion " status")))
      (multiple-value-bind (next-state output)
          (nshell.presentation:reduce-input-state
           state
           (input-key-event :ctrl-l))
        (is (eq :clear-screen output))
        (is-input-state next-state
                        :buffer "git"
                        :cursor-pos 3
                        :completion-index 0
                        :completion-base-buffer "git"
                        :completion-base-cursor 3
                        :last-candidates '("git" "grep")
                        :suggestion " status")
        (setf nshell.presentation::*input-state* next-state)
        (let ((rendered (capture-process-output-event output)))
          (is (search "[2J" rendered))
          (is (search "[1;1H" rendered))))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "git"
                      :cursor-pos 3
                      :completion-index 0
                      :completion-base-buffer "git"
                      :completion-base-cursor 3
                      :last-candidates '("git" "grep")
                      :suggestion " status"))))

(test e2e-ctrl-underscore-undo-input-cycle
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "git statusx" 'list)
                                  (list (code-char 31)))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "git status" line))
    (with-complete-command-line (result ast line)
      (is (string= "git" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("status")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-alt-y-yank-pop-input-cycle
  (let* ((events (read-key-events-from-string
                  (coerce (append (coerce "echo first second" 'list)
                                  (list (code-char 23)
                                        (code-char 23)
                                        (code-char 25))
                                  (coerce (esc-sequence "y") 'list))
                          'string)))
         (state (apply-key-events-to-input-state
                 (input-state)
                 events))
         (line (nshell.presentation:input-state-buffer state)))
    (is (string= "echo second" line))
    (with-complete-command-line (result ast line)
      (is (string= "echo" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("second")
                 (nshell.domain.parsing:command-node-args ast))))))

(test e2e-multiline-quoted-command-cycle
  (let* ((history (nshell.domain.history:make-command-history))
         (line (format nil "echo \"hello~%world\"")))
    (with-complete-command-line (result ast line)
      (is (nshell.domain.parsing:command-node-p ast))
      (is (equal (list (format nil "hello~%world"))
                 (nshell.domain.parsing:command-node-arg-values ast))))
    (nshell.domain.history:history-add history line)
    (is (= 1 (nshell.domain.history:history-size history)))))
(test e2e-pipeline-smoke
  "Verify pipeline execution via spawn-pipeline"
  (let* ((cmd1 (nshell.domain.parsing:make-command-node "echo" '("hello")))
         (pipe (nshell.domain.parsing:make-pipeline-node (list cmd1)))
         (exit (nshell.infrastructure.acl:spawn-pipeline
                (nshell.domain.parsing:pipeline-node-commands pipe))))
    (is (= 0 exit))))

(test e2e-pipeline-redirections-apply-per-stage
  "Pipeline stages should apply their own input and output redirects."
  (with-repl-test-state
    (let* ((root (merge-pathnames (format nil "nshell-pipeline-redir-~d/"
                                          (random 1000000))
                                  (uiop:temporary-directory)))
           (input (merge-pathnames "input.txt" root))
           (output (merge-pathnames "output.txt" root))
           (content "pipeline redirection"))
      (unwind-protect
           (progn
             (ensure-directories-exist root)
             (with-open-file (stream input
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (write-string content stream))
             (let ((line (format nil "cat < ~a | cat > ~a"
                                 (namestring input)
                                 (namestring output))))
               (with-complete-command-line (result ast line)
                (multiple-value-bind (output-text code)
                    (call-repl-execute-ast ast)
                  (declare (ignore output-text))
                  (is (= 0 code)))
                (is (probe-file output))
                 (with-open-file (stream output :direction :input)
                   (let ((actual (make-string (file-length stream))))
                     (read-sequence actual stream)
                     (is (string= content actual)))))))
        (handler-case
            (when (probe-file root)
              (uiop:delete-directory-tree root :validate t))
          (error ()))))))
(test e2e-syntax-error-stops-before-execution
  (with-parsed-command-line (result "| echo should-not-run")
    (is (not (nshell.domain.parsing:parse-complete-p result)))
    (is (eq :missing-command
            (nshell.domain.parsing:parse-diagnostic-kind
             (first (nshell.domain.parsing:parse-errors result)))))))
(test e2e-external-command
  "External command execution returns correct exit code"
  (is (= 0 (nshell.infrastructure.acl:run-external "true" '())))
  (is (not (= 0 (nshell.infrastructure.acl:run-external "false" '())))))
