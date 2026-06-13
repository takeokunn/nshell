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
   (:file "domain/history/entry")
   (:file "domain/history/history")
   (:file "domain/configuration/theme")
   (:file "domain/configuration/config")
   (:file "domain/prompting/prompt")
   (:file "main")))
