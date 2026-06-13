;;; Environment variable model
(in-package #:nshell.domain.environment)

(defstruct (env-var (:constructor make-env-var (name value &optional exported-p)))
  "A shell environment variable."
  (name "" :type string :read-only t)
  (value "" :type string :read-only t)
  (exported-p nil :type boolean :read-only t))

(defstruct (environment (:constructor %make-environment (vars)))
  "A collection of shell environment variables keyed by name."
  (vars (make-hash-table :test #'equal) :type hash-table :read-only t))

(defun make-environment ()
  "Create an empty environment."
  (%make-environment (make-hash-table :test #'equal)))

(defun copy-env-vars (env)
  "Return a shallow copy of ENV's variable table."
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (name var)
               (setf (gethash name copy) var))
             (environment-vars env))
    copy))

(defun make-default-environment ()
  "Create a default shell environment using OS environment variables.
   Falls back to safe defaults when variables are unset (e.g. in Nix sandbox)."
  (let ((env (make-environment))
        (cwd (handler-case (namestring (uiop:getcwd)) (error () "/"))))
    ;; Core variables with fallbacks
    (setf env (env-set env "HOME" (or (uiop:getenv "HOME") "/") t))
    (setf env (env-set env "PATH" (or (uiop:getenv "PATH") "/bin:/usr/bin") t))
    (setf env (env-set env "USER" (or (uiop:getenv "USER") "nobody") t))
    (setf env (env-set env "PWD" (or (uiop:getenv "PWD") cwd) t))
    (setf env (env-set env "SHELL" (or (uiop:getenv "SHELL") "/bin/sh") t))
    (setf env (env-set env "TERM" (or (uiop:getenv "TERM") "dumb") t))
    env))

(defun env-get (env name)
  "Return the value of NAME in ENV, or NIL when it is not defined."
  (let ((var (gethash name (environment-vars env))))
    (when var (env-var-value var))))

(defun env-set (env name value exported)
  "Return ENV updated with NAME set to VALUE.
EXPORTED controls whether the variable appears in ENV-LIST."
  (check-type name string)
  (check-type value string)
  (let ((vars (copy-env-vars env)))
    (setf (gethash name vars) (make-env-var name value (not (null exported))))
    (%make-environment vars)))

(defun env-unset (env name)
  "Return ENV without NAME."
  (check-type name string)
  (let ((vars (copy-env-vars env)))
    (remhash name vars)
    (%make-environment vars)))

(defun env-export (env name)
  "Return ENV with NAME marked exported, preserving its current value."
  (check-type name string)
  (let* ((vars (copy-env-vars env))
         (var (gethash name vars)))
    (when var
      (setf (gethash name vars)
            (make-env-var name (env-var-value var) t)))
    (%make-environment vars)))

(defun env-list (env)
  "Return exported variables in ENV as a list of (NAME . VALUE) pairs."
  (let ((pairs nil))
    (maphash (lambda (name var)
               (when (env-var-exported-p var)
                 (push (cons name (env-var-value var)) pairs)))
             (environment-vars env))
    (sort pairs #'string< :key #'car)))
