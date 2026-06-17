(in-package #:nshell.infrastructure.acl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;;; Syscall integration is split by responsibility:
;;; - syscall-foreign.lisp: alien declarations and platform constants
;;; - syscall-environment.lisp: subprocess environment state
;;; - syscall-redirection.lisp: redirect data helpers
;;; - syscall-process.lisp: single process execution
;;; - syscall-pipeline.lisp: OS pipe execution
;;; - syscall-job-control.lisp: process groups and wait status
;;; - syscall-terminal.lisp: terminal ioctl helpers
