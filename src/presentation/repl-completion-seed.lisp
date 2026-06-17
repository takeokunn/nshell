;;; REPL completion seed data
(in-package #:nshell.presentation)

(defparameter +repl-completion-command-specs+
  '(("ls" :flags ("-l" "-a" "-h" "-R" "--help"))
    ("cd")
    ("echo")
    ("pwd")
    ("exit")
    ("fg")
    ("bg")
    ("jobs")
    ("set" :flags ("-x" "--export" "-e" "--erase" "-q" "--query"))
    ("export")
    ("alias")
    ("abbr" :flags ("-a" "--add" "-p" "--position" "command" "anywhere"
                     "-e" "--erase" "-q" "--query" "-l" "--list" "-s" "--show"))
    ("complete" :flags ("-c" "--command" "-f" "--flag" "-d" "--description"))
    ("type")
    ("which")
    ("test")
    ("[")
    ("string" :flags ("length" "lower" "upper" "join" "split" "replace" "match"
                      "trim" "-a" "--all" "-q" "--quiet" "-i" "--ignore-case"
                      "--"))
    ("source")
    (".")
    ("read" :flags ("-p"))
    ("function" :flags ("-e" "-q"))
    ("true")
    ("false")
    ("contains" :flags ("-i" "--index" "--"))
    ("not")
    ("history" :flags ("search" "delete" "clear" "size" "--prefix" "--contains" "--exact"
                       "--case-sensitive"))
    ("help")
    ("exec")
    ("disown"))
  "Seed command and flag data used to populate the REPL completion knowledge base.")

(defun seed-repl-completion-knowledge-base (knowledge-base)
  (dolist (spec +repl-completion-command-specs+ knowledge-base)
    (destructuring-bind (command &key flags) spec
      (nshell.domain.completion:kb-add-command knowledge-base command :flags flags))))
