;;; nshell.asd - ASDF system definition for nshell
;;; Zero external dependencies, fiveam for testing only

(defsystem "nshell"
  :version "0.1.0"
  :author "nshell contributors"
  :license "MIT"
  :description "Modern interactive shell in Common Lisp"
  :depends-on ()
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:module "domain"
      :depends-on ("package")
      :components
      ((:module "events" :components ((:file "base-event")))
       (:module "signals" :components ((:file "signal")))
       (:module "execution" :components ((:file "command") (:file "pipeline") (:file "job")))
       (:module "parsing" :components ((:file "ast") (:file "tokenizer") (:file "unification") (:file "parser")))
       (:module "completion" :components ((:file "candidate") (:file "knowledge-base") (:file "engine")))
       (:module "history" :components ((:file "entry") (:file "history")))
       (:module "job-control" :components ((:file "monitor")))
       (:module "configuration" :components ((:file "theme") (:file "config")))
       (:module "prompting" :components ((:file "prompt")))))
     (:module "application"
      :depends-on ("package")
      :components ((:file "event-dispatcher") (:file "execute-pipeline") (:file "manage-job") (:file "search-history")))
     (:module "infrastructure"
      :depends-on ("package")
      :components
      ((:module "acl" :components ((:file "syscall") (:file "pty") (:file "signal-acl")))
       (:module "persistence" :components ((:file "file-history") (:file "file-config")))
       (:module "terminal" :components ((:file "raw-mode") (:file "ansi") (:file "screen") (:file "input")))))
     (:module "presentation"
      :depends-on ("package")
      :components ((:file "repl") (:file "prompt-display") (:file "completion-ui") (:file "autosuggest") (:file "highlight")))
     (:file "main" :depends-on ("package"))))))

(defsystem "nshell/test"
  :version "0.1.0"
  :author "nshell contributors"
  :license "MIT"
  :description "Test system for nshell"
  :depends-on ("nshell" "fiveam")
  :components
  ((:module "tests"
    :components
    ((:file "package")
     (:file "test-runner")
     (:module "unit"
      :components
      ((:file "test-domain-events")
       (:file "test-signals")
       (:file "test-execution-domain")
       (:file "test-history-domain")
       (:file "test-configuration")
       (:file "test-tokenizer")
       (:file "test-unification")
       (:file "test-parser")
       (:file "test-cps")
       (:file "test-job-control-domain")
       (:file "test-pipeline-plan")))
     (:module "integration"
      :components
      ((:file "test-process")
       (:file "test-pipeline")
       (:file "test-terminal")
       (:file "test-file-history")))
     (:module "e2e"
      :components
      ((:file "test-smoke")
       (:file "test-history")
       (:file "test-signals")
       (:file "test-job-control")))
     (:module "perf"
      :components
      ((:file "test-startup"))))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam '#:run!
                               (uiop:find-symbol* '#:nshell-tests :nshell/test))))
