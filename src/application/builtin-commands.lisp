(in-package #:nshell.application)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %with-string-parser-result ((&rest vars) parser-form &body body)
    (let ((error-var (car (last vars))))
      `(multiple-value-bind ,vars ,parser-form
         (if ,error-var
             (values ,error-var 1)
             (progn ,@body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-builtin (name lambda-list ignore-variables &body body)
    `(defun ,name ,lambda-list
       ,@(when ignore-variables
           `((declare (ignore ,@ignore-variables))))
       ,@body)))

(define-builtin %builtin-string (context args) (context)
  (%builtin-string-dispatch args))

(define-builtin %builtin-echo (context args) (context)
  (values (format nil "~{~a~^ ~}~%" args) 0))

(define-builtin %builtin-pwd (context args) (args)
  (values (format nil "~a~%" (namestring (funcall (%filesystem-fn context :cwd)))) 0))

(define-builtin %builtin-ls (context args) (args)
  (handler-case
      (values
       (with-output-to-string (out)
         (dolist (file (funcall (%filesystem-fn context :list-dir)
                                (funcall (%filesystem-fn context :cwd))))
           (format out "~a~%" (file-namestring file))))
       0)
    (error (condition)
      (values (format nil "ls: ~a~%" condition) 1))))

(defun %builtin-cd (context args)
  (handler-case
      (progn
        (when args
          (funcall (%filesystem-fn context :chdir) (first args)))
        (values nil 0))
    (error (condition)
      (values (format nil "cd: ~a~%" condition) 1))))

(define-builtin %builtin-exit (context args) (args)
  (setf (shell-context-running context) nil)
  (values nil 0))

(define-builtin %builtin-true (context args) (context args)
  (values nil 0))

(define-builtin %builtin-false (context args) (context args)
  (values nil 1))

(defun %invert-status-code (code)
  (if (zerop (or code 0)) 1 0))

(defun %builtin-not (context args)
  (if (null args)
      (%builtin-usage "not" "not command [args...]" 2)
      (let* ((command (first args))
             (command-args (rest args)))
        (multiple-value-bind (output code)
            (%execute-command-by-name-in-context context command command-args)
          (values output (%invert-status-code code))))))

(defun %builtin-exec (context args)
  (declare (ignore context))
  (if args
      (progn
        (sb-ext:quit :unix-status
                     (handler-case
                         (sb-ext:process-exit-code
                          (sb-ext:run-program (first args) (rest args)
                            :input *standard-input*
                            :output *standard-output*
                            :error *error-output*
                            :wait t
                            :search t))
                       (error (e)
                          (format *error-output* "exec: ~a: ~a~%" (first args) e)
                         1))))
      (%builtin-usage "exec" "exec command [args...]")))

(defparameter +contains-option-specs+
  '(("-i" :index-p t)
    ("--index" :index-p t)))

(defun %contains-usage ()
  (%builtin-usage
   "contains"
   (%builtin-usage-clauses-summary +builtin-contains-usage-clauses+)))

(defun %contains-option-spec (option)
  (cdr (assoc option +contains-option-specs+ :test #'string=)))

(defun %parse-contains-args (args)
  (let ((index-p nil)
        (remaining args))
    (%with-option-arguments (remaining option)
        (return)
        (return-from %parse-contains-args
          (values nil nil
                  (format nil "contains: unknown option ~a~%" option)))
        (return)
      ((%contains-option-spec option)
       (setf index-p t
             remaining (rest remaining))))
    (values index-p remaining nil)))

(defun %contains-match-indexes (needle values)
  (loop for value in values
        for index from 1
        when (string= needle value)
          collect index))

(defun %builtin-contains (context args)
  (declare (ignore context))
  (multiple-value-bind (index-p operands error-output)
      (%parse-contains-args args)
    (cond
      (error-output
       (values error-output 2))
      ((null operands)
       (values (%contains-usage) 2))
      (t
       (let* ((needle (first operands))
              (values (rest operands))
              (indexes (%contains-match-indexes needle values)))
         (values (if index-p
                     (with-output-to-string (out)
                       (dolist (index indexes)
                         (format out "~d~%" index)))
                     nil)
                 (if indexes 0 1)))))))

(defparameter +builtin-help-entries+
  (list
   (list :command "echo"
         :synopsis "echo [string ...]"
         :description "print arguments")
   (list :command "pwd"
         :synopsis "pwd"
         :description "print working directory")
   (list :command "ls"
         :synopsis "ls"
         :description "list directory contents")
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
         :description "manage variables")
   (list :command "export"
         :synopsis "export name"
         :description "export variable to environment")
   (list :command "alias"
         :synopsis "alias [name expansion...] | alias -e name... | alias -q name..."
         :description "manage aliases")
   (list :command "abbr"
         :synopsis "abbr [-a [-p command|anywhere] name expansion...] [-e name...] [-q name...] [-l] [-s]"
         :description "manage abbreviations")
   (list :command "complete"
         :synopsis "complete -c command [-f flag ...] [-d description]"
         :description "define completions")
   (list :command "type"
         :synopsis "type name [...]"
         :description "show command type")
   (list :command "which"
         :synopsis "which name [...]"
         :description "show command path")
   (list :command "test"
         :synopsis "test expression"
         :description "evaluate conditional")
   (list :command "["
         :synopsis "[ expression ]"
         :description "evaluate conditional")
   (list :command "string"
         :synopsis (format nil "~a ...; ~a ..."
                           (%builtin-string-subcommand-summary)
                           (%builtin-string-manipulation-summary))
         :description "manipulate strings")
   (list :command "source"
         :synopsis "source file"
         :description "execute commands from file")
   (list :command "."
         :synopsis ". file"
         :description "execute commands from file")
   (list :command "read"
         :synopsis "read [-p prompt] variable"
         :description "read line of input")
   (list :command "function"
         :synopsis "function [name body... end] | function -e name... | function -q name..."
         :description "manage functions")
   (list :command "history"
         :synopsis (%builtin-usage-clauses-summary +builtin-history-usage-clauses+)
         :description "show and manage command history")
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
         :synopsis (%builtin-usage-clauses-summary +builtin-contains-usage-clauses+)
         :description "test whether a value is present")
   (list :command "not"
         :synopsis "not command [args...]"
         :description "invert command status")))

(defun %builtin-help-entry (command)
  (find command +builtin-help-entries+
        :key (lambda (entry) (getf entry :command))
        :test #'string=))

(defun %builtin-help-entry-line (entry)
  (format nil "~a - ~a"
          (getf entry :synopsis)
          (getf entry :description)))

(defun %builtin-help-overview ()
  (values
   (with-output-to-string (out)
     (format out "nshell builtin commands:~%")
     (dolist (entry +builtin-help-entries+)
       (format out "  ~a~%" (%builtin-help-entry-line entry))))
   0))

(defun %builtin-help-message (command)
  (let ((entry (%builtin-help-entry command)))
    (if entry
        (values (format nil "~a~%" (%builtin-help-entry-line entry)) 0)
        (values (format nil "help: no help for ~a~%" command) 1))))

(defun %builtin-help (context args)
  (declare (ignore context))
  (if args
      (%builtin-help-message (first args))
      (%builtin-help-overview)))
