(in-package #:nshell.infrastructure.acl)

(defvar *exported-environment* nil
  "List of \"KEY=VALUE\" strings for exported environment variables.
Set by the REPL from the current shell environment.")

(defun %get-environment ()
  "Return the exported environment list for subprocess execution."
  (or *exported-environment* nil))
