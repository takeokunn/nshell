(in-package #:nshell.application)

(defmacro define-builtin-table (variable &body entries)
  `(defparameter ,variable
     (list ,@(mapcar (lambda (entry)
                       (destructuring-bind (name handler) entry
                         `(cons ,name #',handler)))
                     entries))))

(defmacro define-command-path-builtin (function-name command-name)
  `(defun ,function-name (context args)
     (%builtin-command-path context args ,command-name)))

(defparameter +command-path-builtin-specs+
  '(("type"
     :builtin-format "~a is a shell builtin~%"
     :path-format "~a is ~a~%"
     :missing-prefix "type"
     :missing-format "~a: not found"
     :usage "type name [name ...]")
    ("which"
     :builtin-format "~a: shell built-in command~%"
     :path-format "~a~%"
     :missing-prefix "which"
     :missing-format "no ~a in PATH"
     :usage "which name [name ...]")))

(defun %command-path-spec (command)
  (cdr (assoc command +command-path-builtin-specs+ :test #'string=)))

(defun %describe-command-path (context command missing-formatter)
  (multiple-value-bind (kind location) (resolve-command-path context command)
    (case kind
      (:builtin (values :builtin command))
      (:path (values :path location))
      (otherwise (values nil (funcall missing-formatter command))))))

(defun %format-command-path-missing (spec command)
  (format nil "~a: ~a~%"
          (getf spec :missing-prefix)
          (format nil (getf spec :missing-format) command)))

(defun %builtin-command-path (context args command)
  (let ((spec (%command-path-spec command)))
    (if args
        (let ((exit-code 0))
          (values
           (with-output-to-string (out)
             (dolist (name args)
               (multiple-value-bind (kind text)
                   (%describe-command-path
                    context name
                    (lambda (missing-name)
                      (%format-command-path-missing spec missing-name)))
                 (case kind
                   (:builtin
                    (format out (getf spec :builtin-format) name))
                   (:path
                    (format out (getf spec :path-format) name text))
                   (otherwise
                    (setf exit-code 1)
                    (write-string text out))))))
           exit-code))
        (%builtin-usage command (getf spec :usage)))))

(define-command-path-builtin %builtin-type "type")
(define-command-path-builtin %builtin-which "which")

(define-builtin-table +default-builtin-specs+
  ("echo" %builtin-echo)
  ("pwd" %builtin-pwd)
  ("ls" %builtin-ls)
  ("cd" %builtin-cd)
  ("exit" %builtin-exit)
  ("fg" %builtin-fg)
  ("bg" %builtin-bg)
  ("jobs" %builtin-jobs)
  ("set" %builtin-set)
  ("export" %builtin-export)
  ("alias" %builtin-alias)
  ("abbr" %builtin-abbr)
  ("complete" %builtin-complete)
  ("type" %builtin-type)
  ("which" %builtin-which)
  ("test" %builtin-test)
  ("[" %builtin-bracket)
  ("string" %builtin-string)
  ("source" %builtin-source)
  ("." %builtin-source)
  ("read" %builtin-read)
  ("function" %builtin-function)
  ("true" %builtin-true)
  ("false" %builtin-false)
  ("contains" %builtin-contains)
  ("not" %builtin-not)
  ("history" %builtin-history)
  ("help" %builtin-help)
  ("exec" %builtin-exec)
  ("disown" %builtin-disown))

(defun register-default-builtins ()
  "Register nshell's default builtin command handlers."
  (dolist (entry +default-builtin-specs+ *builtin-registry*)
    (register-builtin (car entry) (cdr entry))))

(register-default-builtins)

(defun expand-command-alias-node (command-node alias-table)
  (if (nshell.domain.parsing:command-node-p command-node)
      (let* ((command (nshell.domain.parsing:command-node-command command-node))
             (alias (gethash command alias-table)))
        (if alias
            (nshell.domain.parsing:with-complete-command-line (result alias-node alias)
              (if (nshell.domain.parsing:command-node-p alias-node)
                  (nshell.domain.parsing:make-command-node
                   (nshell.domain.parsing:command-node-command alias-node)
                   (append (nshell.domain.parsing:command-node-args alias-node)
                           (nshell.domain.parsing:command-node-args command-node)))
                  command-node))
            command-node))
      command-node))
