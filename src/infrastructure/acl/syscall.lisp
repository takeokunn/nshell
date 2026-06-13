(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun run-external (cmd args)
  "Run an external command, print its output, return exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :output :stream :error :output :wait t :search t)))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out
              (loop for line = (read-line out nil nil)
                    while line do (write-line line))))
          (sb-ext:process-exit-code proc)))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      1)))

(defun spawn-pipeline (commands)
  "Execute commands connected by real OS pipes using sb-posix:pipe."
  (let* ((n (length commands))
         (fds nil)
         (procs nil))
    ;; Create pipes
    (dotimes (i (1- n))
      (multiple-value-bind (r w) (sb-posix:pipe)
        (push (list r w) fds)))
    (setf fds (nreverse fds))
    ;; Execute each command with proper pipe connections
    (loop for i from 0 below n
          for cmd-node in commands
          for cmd = (nshell.domain.parsing:command-node-command cmd-node)
          for args = (nshell.domain.parsing:command-node-args cmd-node)
          do (let* ((in-fd (if (> i 0) (first (nth (1- i) fds)) nil))
                    (out-fd (if (< i (1- n)) (second (nth i fds)) nil))
                    (proc (handler-case
                              (sb-ext:run-program cmd args
                                :input (if in-fd in-fd t)
                                :output (if out-fd out-fd :stream)
                                :error :output
                                :wait nil :search t)
                            (error (err)
                              (format *error-output* "nshell: ~a: ~a~%" cmd err)
                              nil))))
               (when proc (push proc procs))
               ;; Close parent's copy of pipe fds
               (when in-fd (sb-posix:close in-fd))
               (when out-fd (sb-posix:close out-fd))))
    ;; Drain last process output and wait
    (let ((exit 0))
      (dolist (proc (reverse procs))
        (sb-ext:process-wait proc)
        (setf exit (or (sb-ext:process-exit-code proc) 0))
        (let ((out (sb-ext:process-output proc)))
          (when (and out (streamp out) (open-stream-p out))
            (handler-case
                (loop for line = (read-line out nil nil)
                      while line do (write-line line))
              (error ())))))
      exit)))

(defun wait-job (process)
  (sb-ext:process-wait process)
  (sb-ext:process-exit-code process))
