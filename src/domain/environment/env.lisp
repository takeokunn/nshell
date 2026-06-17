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

(defun env-set (env name value exported)
  "Return ENV updated with NAME set to VALUE.
EXPORTED controls whether the variable appears in ENV-LIST."
  (check-type name string)
  (check-type value string)
  (let ((vars (copy-env-vars env)))
    (setf (gethash name vars) (make-env-var name value (not (null exported))))
    (%make-environment vars)))

(defun make-default-environment ()
  "Create a default environment with fallback values.
   Pure domain function - callers should provide OS values via inject-os-environment."
  (let ((env (make-environment)))
    (setf env (env-set env "HOME" "/" t))
    (setf env (env-set env "PATH" "/bin:/usr/bin" t))
    (setf env (env-set env "USER" "nobody" t))
    (setf env (env-set env "PWD" "/" t))
    (setf env (env-set env "SHELL" "/bin/sh" t))
    (setf env (env-set env "TERM" "dumb" t))
    env))

(defun env-get (env name)
  "Return the value of NAME in ENV, or NIL when it is not defined."
  (let ((var (gethash name (environment-vars env))))
    (when var (env-var-value var))))

(defun inject-os-environment (env)
  "Inject OS environment values into ENV. Used by infrastructure layer.
   Returns a new environment with OS values overwriting defaults."
  (let ((result env))
    (setf result (env-set result "HOME" (or (uiop:getenv "HOME") (env-get result "HOME")) t))
    (setf result (env-set result "PATH" (or (uiop:getenv "PATH") (env-get result "PATH")) t))
    (setf result (env-set result "USER" (or (uiop:getenv "USER") (env-get result "USER")) t))
    (setf result (env-set result "PWD" (handler-case (namestring (uiop:getcwd)) (error () (env-get result "PWD"))) t))
    (setf result (env-set result "SHELL" (or (uiop:getenv "SHELL") (env-get result "SHELL")) t))
    (setf result (env-set result "TERM" (or (uiop:getenv "TERM") (env-get result "TERM")) t))
    result))

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

(defun env-bindings (env)
  "Return all variables in ENV sorted by name."
  (let ((vars nil))
    (maphash (lambda (name var)
               (declare (ignore name))
               (push var vars))
             (environment-vars env))
    (sort vars #'string< :key #'env-var-name)))

(defun env-list (env)
  "Return exported variables in ENV as a list of (NAME . VALUE) pairs."
  (let ((pairs nil))
    (maphash (lambda (name var)
               (when (env-var-exported-p var)
                 (push (cons name (env-var-value var)) pairs)))
             (environment-vars env))
    (sort pairs #'string< :key #'car)))
