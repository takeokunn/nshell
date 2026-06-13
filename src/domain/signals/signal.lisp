;;; Signal value objects - OS-independent domain model
(in-package #:nshell.domain.signals)

(export '(signal-p
          +sigquit+
          +sigkill+
          +sigwinch+))

;; Use os-signal struct name to avoid package-lock conflict with cl:signal
(defstruct (os-signal (:constructor make-signal (name number))
                      (:predicate signal-p))
  "A POSIX signal as a domain value object.
NAME is a keyword like :SIGINT, :SIGTERM.
NUMBER is the signal number (1-31 typically)."
  (name nil :type keyword :read-only t)
  (number 0 :type (integer 0 64) :read-only t))

;; Wrapper accessors matching the exported API from package.lisp
(defun signal-name (sig) (os-signal-name sig))
(defun signal-number (sig) (os-signal-number sig))

(defun signal= (a b)
  "Value equality for signals."
  (and (signal-p a) (signal-p b)
       (eq (signal-name a) (signal-name b))
       (= (signal-number a) (signal-number b))))

;;; Known signal constants
;;; Use defvar instead of defconstant to avoid compile-time evaluation issues
(defvar +sigint+  (load-time-value (make-signal :sigint 2)))
(defvar +sigquit+ (load-time-value (make-signal :sigquit 3)))
(defvar +sigkill+ (load-time-value (make-signal :sigkill 9)))
(defvar +sigterm+ (load-time-value (make-signal :sigterm 15)))
(defvar +sigtstp+ (load-time-value (make-signal :sigtstp 20)))
(defvar +sigcont+ (load-time-value (make-signal :sigcont 18)))
(defvar +sigchld+ (load-time-value (make-signal :sigchld 17)))
(defvar +sigwinch+ (load-time-value (make-signal :sigwinch 28)))
