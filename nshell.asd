(defsystem "nshell"
  :version "0.1.0"
  :author "nshell contributors"
  :license "MIT"
  :description "Modern interactive shell in Common Lisp"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "domain/events/base-event")
   (:file "domain/events/command-events")
   (:file "domain/events/job-events")
   (:file "domain/events/signal-events")
   (:file "domain/signals/signal")
   (:file "domain/execution/command")
   (:file "domain/execution/pipeline")
   (:file "domain/execution/job")
   (:file "domain/parsing/ast")
   (:file "domain/parsing/tokenizer")
   (:file "domain/parsing/unification")
   (:file "domain/parsing/parser")
   (:file "domain/completion/candidate")
   (:file "domain/completion/knowledge-base")
   (:file "domain/completion/engine")
   (:file "domain/history/entry")
   (:file "domain/history/history")
   (:file "domain/job-control/monitor")
   (:file "domain/configuration/theme")
   (:file "domain/configuration/config")
   (:file "domain/prompting/prompt")
   (:file "application/event-dispatcher")
   (:file "application/execute-pipeline")
   (:file "application/manage-job")
   (:file "application/search-history")
   (:file "infrastructure/acl/syscall")
   (:file "infrastructure/acl/pty")
   (:file "infrastructure/acl/signal-acl")
   (:file "infrastructure/persistence/file-history")
   (:file "infrastructure/persistence/file-config")
   (:file "infrastructure/terminal/raw-mode")
   (:file "infrastructure/terminal/ansi")
   (:file "infrastructure/terminal/screen")
   (:file "infrastructure/terminal/input")
   (:file "presentation/repl")
   (:file "presentation/prompt-display")
   (:file "presentation/completion-ui")
   (:file "presentation/autosuggest")
   (:file "presentation/highlight")
   (:file "main")))

(defsystem "nshell/test"
  :version "0.1.0"
  :author "nshell contributors"
  :license "MIT"
  :description "Test system for nshell"
  :depends-on ("nshell" "fiveam")
  :pathname "tests"
  :serial t
  :components
  ((:file "package")
   (:file "test-runner")
   (:file "unit/test-domain-events")
   (:file "unit/test-signals")
   (:file "unit/test-execution-domain")
   (:file "unit/test-history-domain")
   (:file "unit/test-configuration")
   (:file "unit/test-tokenizer")
   (:file "unit/test-unification")
   (:file "unit/test-parser")
   (:file "unit/test-cps")
   (:file "unit/test-job-control-domain")
   (:file "unit/test-pipeline-plan")
   (:file "integration/test-pipeline")
   (:file "integration/test-process")
   (:file "integration/test-terminal")
   (:file "integration/test-file-history")
   (:file "e2e/test-smoke")
   (:file "e2e/test-history")
   (:file "e2e/test-signals")
   (:file "e2e/test-job-control")
   (:file "perf/test-startup"))
  :perform (test-op (o s)
             (declare (ignore o s))
             (uiop:symbol-call :fiveam '#:run!
                               (uiop:find-symbol* '#:nshell-tests :nshell/test))))
