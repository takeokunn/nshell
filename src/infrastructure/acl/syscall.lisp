(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defun run-external (cmd args)
  (handler-case
      (let ((proc (sb-ext:run-program cmd args :output :stream :error :output :wait t :search t)))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out (loop for line = (read-line out nil nil) while line do (write-line line))))
          (or (sb-ext:process-exit-code proc) 0)))
    (error (err) (format *error-output* "nshell: ~a: ~a~%" cmd err) 1)))

(defun spawn-pipeline (commands)
  (let* ((n (length commands)) (procs nil) (prev-output nil))
    (loop for i from 0 below n for cmd-node in commands
          for cmd = (nshell.domain.parsing:command-node-command cmd-node)
          for args = (nshell.domain.parsing:command-node-args cmd-node)
          do (let ((proc (handler-case (sb-ext:run-program cmd args :input (if prev-output :stream t) :output :stream :error :output :wait nil :search t)
                            (error (err) (format *error-output* "nshell: ~a: ~a~%" cmd err) nil))))
               (when proc
                 (when prev-output (let ((in (sb-ext:process-input proc))) (when in (handler-case (loop for line = (read-line prev-output nil nil) while line do (write-line line in)) (error ())) (close in))))
                 (push proc procs) (setf prev-output (sb-ext:process-output proc)))))
    (let ((exit 0))
      (dolist (proc (reverse procs))
        (when prev-output (handler-case (loop for line = (read-line prev-output nil nil) while line do (write-line line)) (error ())) (setf prev-output nil))
        (sb-ext:process-wait proc) (setf exit (or (sb-ext:process-exit-code proc) 0)))
      exit)))

(defvar *redirected-stdout* nil)
(defvar *redirected-stdin* nil)

(defun redirect-output (filename mode)
  (let ((stream (open filename :direction :output :if-exists mode :if-does-not-exist :create)))
    (setf *redirected-stdout* *standard-output*) (setf *standard-output* stream)))

(defun redirect-input (filename)
  (let ((stream (open filename :direction :input :if-does-not-exist :error)))
    (setf *redirected-stdin* *standard-input*) (setf *standard-input* stream)))

(defun restore-redirects ()
  (when *redirected-stdout* (close *standard-output*) (setf *standard-output* *redirected-stdout*) (setf *redirected-stdout* nil))
  (when *redirected-stdin* (close *standard-input*) (setf *standard-input* *redirected-stdin*) (setf *redirected-stdin* nil)))
