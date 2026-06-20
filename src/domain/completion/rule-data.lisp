(in-package #:nshell.domain.completion)

(defstruct fact
  (predicate nil :type symbol :read-only t)
  (args '() :type list :read-only t))

(defstruct rule
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defstruct (rule-knowledge-base (:constructor make-rule-knowledge-base (&key (facts nil) (rules nil))))
  (facts nil :type list)
  (rules nil :type list))

(defun make-fact-from-spec (spec)
  (make-fact :predicate (first spec) :args (rest spec)))

(defun make-rule-from-spec (spec)
  (make-rule :head (first spec) :body (rest spec)))

(defparameter *max-proof-depth* 32
  "Maximum rule-expansion depth for completion proof search.")

(defparameter +builtin-command-catalog+
  (list
   (list :command "echo"
         :synopsis "echo [string ...]"
         :description "print arguments")
   (list :command "pwd"
         :synopsis "pwd"
         :description "print working directory")
   (list :command "ls"
         :synopsis "ls"
         :description "list directory contents"
         :flags '("-l" "-a" "-h" "-R" "--help"))
   (list :command "cd"
         :synopsis "cd [dir]"
         :description "change directory")
   (list :command "exit"
         :synopsis "exit"
         :description "exit the shell")
   (list :command "fg"
         :synopsis "fg [job-id]"
         :description "bring job to foreground")
   (list :command "bg"
         :synopsis "bg [job-id]"
         :description "resume job in background")
   (list :command "jobs"
         :synopsis "jobs"
         :description "list jobs")
   (list :command "disown"
         :synopsis "disown [job-id]"
         :description "remove job from job list")
   (list :command "set"
         :synopsis "set [-x|--export] name value... | set [-e|--erase] name... | set [-q|--query] name..."
         :description "manage variables"
         :flags '("-x" "--export" "-e" "--erase" "-q" "--query"))
   (list :command "export"
         :synopsis "export name"
         :description "export variable to environment")
   (list :command "alias"
         :synopsis "alias [name expansion...] | alias -e name... | alias -q name..."
         :description "manage aliases")
   (list :command "abbr"
         :synopsis "abbr [-a [-p command|anywhere] name expansion...] [-e name...] [-q name...] [-l] [-s]"
         :description "manage abbreviations"
         :flags '("-a" "--add" "-p" "--position" "command" "anywhere"
                  "-e" "--erase" "-q" "--query" "-l" "--list" "-s" "--show"))
   (list :command "complete"
         :synopsis "complete -c command [-f flag ...] [-d description]"
         :description "define completions"
         :flags '("-c" "--command" "-f" "--flag" "-d" "--description"))
   (list :command "type"
         :synopsis "type [OPTIONS] NAME [...]"
         :description "show command type"
         :flags '("-a" "--all" "-s" "--short" "-f" "--no-functions"
                  "--color" "-q" "--query" "--quiet" "-p" "--path" "-P" "--force-path"
                  "-t" "--type" "-h" "--help"))
   (list :command "which"
         :synopsis "which NAME [...]"
         :description "show command path")
   (list :command "test"
         :synopsis "test expression"
         :description "evaluate conditional")
   (list :command "["
         :synopsis "[ expression ]"
         :description "evaluate conditional")
   (list :command "string"
         :synopsis "string collect|length|lower|upper|join|split|replace|match|repeat|sub|trim ...; string replace|match|repeat|sub|trim ..."
         :description "manipulate strings"
         :flags '("length" "lower" "upper" "join" "split" "replace" "match"
                  "trim" "-a" "--all" "-q" "--quiet" "-i" "--ignore-case"
                  "--"))
   (list :command "source"
         :synopsis "source file"
         :description "execute commands from file")
   (list :command "."
         :synopsis ". file"
         :description "execute commands from file")
   (list :command "read"
         :synopsis "read [-p prompt] variable"
         :description "read line of input"
         :flags '("-p"))
   (list :command "function"
         :synopsis "function [name body... end] | function -e name... | function -q name..."
         :description "manage functions"
         :flags '("-e" "-q"))
   (list :command "history"
         :synopsis "history [search [--prefix|--contains|--exact|--case-sensitive] query | delete command | clear | size]"
         :description "show and manage command history"
         :flags '("search" "delete" "clear" "size" "--prefix" "--contains" "--exact"
                  "--case-sensitive"))
   (list :command "help"
         :synopsis "help [command]"
         :description "show help")
   (list :command "exec"
         :synopsis "exec command [args...]"
         :description "replace shell with command")
   (list :command "true"
         :synopsis "true"
         :description "return success")
   (list :command "false"
         :synopsis "false"
         :description "return failure")
   (list :command "contains"
         :synopsis "contains [-i|--index] string [values...]"
         :description "test whether a value is present"
         :flags '("-i" "--index" "--"))
   (list :command "count"
         :synopsis "count [values...]"
         :description "print the number of arguments"
         :flags '())
   (list :command "not"
         :synopsis "not command [args...]"
         :description "invert command status")))

(defun builtin-command-flag-facts ()
  "Return static flag facts derived from the builtin command catalog."
  (mapcan (lambda (entry)
            (let ((command (getf entry :command))
                  (flags (getf entry :flags)))
              (mapcar (lambda (flag)
                        (list 'has-flag command flag))
                      flags)))
          +builtin-command-catalog+))

(defparameter +command-path-builtin-specs+
  '(("type"
     :builtin-format "~a is a shell builtin~%"
     :path-format "~a is ~a~%"
     :missing-prefix "type"
     :missing-format "~a: not found~%"
     :usage "type [OPTIONS] NAME [...]")
    ("which"
     :builtin-format "~a: shell built-in command~%"
     :path-format "~a~%"
     :missing-prefix "which"
     :missing-format "no ~a in PATH"
     :usage "which NAME [NAME ...]")))

(defparameter +type-builtin-spec+
  '(:alias-format "~a is an alias for ~a~%"
    :function-format "~a is a function~%"
    :abbreviation-format "~a is an abbreviation for ~a~%"
    :builtin-format "~a is a shell builtin~%"
    :path-builtin-format "~a is a builtin~%"
    :path-format "~a is ~a~%"
    :path-only-format "~a~%"
    :missing-format "~a: not found~%"
    :usage "type [OPTIONS] NAME [...]"))

(defun builtin-help-entries ()
  "Return the canonical builtin help entries."
  (mapcar (lambda (entry)
            (list :command (getf entry :command)
                  :synopsis (getf entry :synopsis)
                  :description (getf entry :description)))
          +builtin-command-catalog+))

(defun builtin-completion-command-specs ()
  "Return the canonical REPL completion seed derived from builtin help entries."
  (mapcar (lambda (entry)
            (let ((command (getf entry :command))
                  (flags (getf entry :flags))
                  (description (getf entry :description)))
              (append (list command)
                      (when flags (list :flags flags))
                      (when description (list :description description)))))
          +builtin-command-catalog+))

(defun builtin-rule-facts ()
  "Return the static facts used to seed builtin completion knowledge."
  (append
   '((command-is "cd" "cd")
     (command-is "source" "source")
     (command-is "." "source"))
   (mapcan (lambda (entry)
             (let ((command (getf entry :command))
                   (description (getf entry :description)))
               (list (list 'completes command command)
                     (list 'describes command description))))
           +builtin-command-catalog+)
   (builtin-command-flag-facts)
   '((describes "git" "distributed version control")
     (describes "--help" "show command help")
     (describes "add" "stage changes")
     (describes "commit" "record changes")
     (describes "checkout" "switch branches or restore paths")
     (describes "status" "show working tree status")
     (has-flag "ls" "--help")
     (has-flag "git" "--help")
     (git-subcommand "add")
     (git-subcommand "commit")
     (git-subcommand "checkout")
     (git-subcommand "status"))))

(defun builtin-rule-rules ()
  "Return the static rule forms used by the builtin completion knowledge base."
  '(((suggests-dir ?input)
     (command-is ?input "cd"))
    ((completes ?cmd "--help")
     (has-flag ?cmd "--help"))
    ((completes ?cmd ?flag)
     (has-flag ?cmd ?flag))
    ((completes "git" ?sub)
     (git-subcommand ?sub))
    ((suggests-file ?input)
     (command-is ?input "source"))))

(defgeneric predicate-true-p (predicate args bindings)
  (:documentation "Return true when PREDICATE with walked ARGS is true in the current environment."))

(defmethod predicate-true-p ((predicate symbol) args bindings)
  (declare (ignore predicate args bindings))
  nil)

(defun assert-fact! (kb fact)
  (push fact (rule-knowledge-base-facts kb))
  kb)

(defun assert-rule! (kb rule)
  (push rule (rule-knowledge-base-rules kb))
  kb)

(defun logic-variable-symbol-p (x)
  (and (symbolp x)
       (< 0 (length (symbol-name x)))
       (char= #\? (char (symbol-name x) 0))))

(defun variable-name (symbol)
  (subseq (symbol-name symbol) 1))

(defun convert-logic-variables (form env)
  (cond
    ((logic-variable-symbol-p form)
     (or (gethash form env)
         (setf (gethash form env)
               (nshell.domain.parsing:make-var (variable-name form)))))
    ((consp form)
     (cons (convert-logic-variables (car form) env)
           (convert-logic-variables (cdr form) env)))
    (t form)))

(defun fact-head (fact env)
  (convert-logic-variables (cons (fact-predicate fact) (fact-args fact)) env))

(defun rule-head-term (rule env)
  (convert-logic-variables (rule-head rule) env))

(defun rule-body-terms (rule env)
  (mapcar (lambda (goal) (convert-logic-variables goal env))
          (rule-body rule)))

(defun prove-body (kb goals bindings depth)
  (if (null goals)
      (list bindings)
      (loop for solution in (prove-internal kb (first goals) bindings depth)
            append (prove-body kb (rest goals) solution depth))))

(defun prove-built-in-solutions (goal bindings)
  (let* ((predicate (first goal))
         (args (mapcar (lambda (arg) (nshell.domain.parsing:walk arg bindings))
                       (rest goal))))
    (when (predicate-true-p predicate args bindings)
      (list bindings))))

(defun prove-internal (kb goal bindings depth)
  (let ((solutions '()))
    (dolist (fact (rule-knowledge-base-facts kb))
      (let ((candidate (nshell.domain.parsing:unify goal (fact-head fact (make-hash-table :test #'eq)) bindings)))
        (when (nshell.domain.parsing:unify-p candidate)
          (push candidate solutions))))
    (when (plusp depth)
      (dolist (rule (rule-knowledge-base-rules kb))
        (let* ((env (make-hash-table :test #'eq))
               (head (rule-head-term rule env))
               (body (rule-body-terms rule env))
               (head-bindings (nshell.domain.parsing:unify goal head bindings)))
          (when (nshell.domain.parsing:unify-p head-bindings)
            (setf solutions
                  (append (prove-body kb body head-bindings (1- depth))
                          solutions))))))
    (append (nreverse solutions)
            (prove-built-in-solutions goal bindings))))

(defun externalize-bindings (env bindings)
  (let ((result '()))
    (maphash (lambda (symbol var)
               (let ((value (nshell.domain.parsing:walk var bindings)))
                 (unless (nshell.domain.parsing:var-p value)
                   (push (cons symbol value) result))))
             env)
    (nreverse result)))

(defun prove (kb goal &optional (bindings '()) (max-depth *max-proof-depth*))
  (let* ((env (make-hash-table :test #'eq))
         (internal-goal (convert-logic-variables goal env)))
    (mapcar (lambda (solution)
              (externalize-bindings env solution))
            (prove-internal kb internal-goal bindings max-depth))))

(defun prove-all (kb goal &key (max-depth *max-proof-depth*))
  (prove kb goal '() max-depth))

(defparameter *built-in-rule-knowledge-base*
  (make-rule-knowledge-base
   :facts (mapcar #'make-fact-from-spec
                  (builtin-rule-facts))
   :rules (mapcar #'make-rule-from-spec
                  (builtin-rule-rules))))
