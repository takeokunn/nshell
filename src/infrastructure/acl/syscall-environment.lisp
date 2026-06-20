(in-package #:nshell.infrastructure.acl)

(defvar *exported-environment* nil
  "List of \"KEY=VALUE\" strings for exported environment variables.
Set by the REPL from the current shell environment.")

(defun %get-environment ()
  "Return the environment list for subprocess execution. When the shell has
exported variables, use them; otherwise inherit the real process environment so
child processes still receive a PATH (and can be found via :search)."
  (or *exported-environment*
      #+sbcl (sb-ext:posix-environ)
      #-sbcl nil))
