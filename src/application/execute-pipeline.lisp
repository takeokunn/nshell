(in-package #:nshell.application)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun execute-command-line (line history dispatcher)
  (declare (ignore dispatcher))
  (let ((result (nshell.domain.parsing:parse-command-line line)))
    (when (nshell.domain.parsing:parse-complete-p result)
      (nshell.domain.history:history-add history line)
      (let ((ast (nshell.domain.parsing:parse-result-ast result)))
        (values ast result)))))

(defun execute-external (cmd args)
  "Execute an external command and return the exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :output :stream :error :output :wait t :search t)))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out
              (loop for line = (read-line out nil nil)
                    while line
                    do (write-line line))))
          (sb-ext:process-exit-code proc)))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      1)))

(defun execute-pipeline-commands (commands)
  "Execute commands connected by pipes using OS pipe mechanism."
  (let* ((n (length commands))
         (pipes '())
         (processes '()))
    ;; Create pipes between commands
    (dotimes (i (1- n))
      (push (sb-posix:pipe) pipes))
    (setf pipes (nreverse pipes))
    ;; Execute each command
    (loop for i from 0 below n
          for cmd-node in commands
          for cmd = (nshell.domain.parsing:command-node-command cmd-node)
          for args = (nshell.domain.parsing:command-node-args cmd-node)
          for in-fd = (if (> i 0) (first (nth (1- i) pipes)) 0)
          for out-fd = (if (< i (1- n)) (second (nth i pipes)) 1)
          for proc = (handler-case
                         (let ((p (sb-ext:run-program cmd args
                                   :input (if (= in-fd 0) t in-fd)
                                   :output (if (= out-fd 1) :stream out-fd)
                                   :error :output
                                   :wait nil :search t)))
                           ;; Close pipe fds in parent
                           (when (> i 0) (sb-posix:close (first (nth (1- i) pipes))))
                           (when (< i (1- n)) (sb-posix:close (second (nth i pipes))))
                           p)
                       (error (err)
                         (format *error-output* "nshell: ~a: ~a~%" cmd err)
                         nil)))
          when proc do (push proc processes))
    ;; Wait for all and get last exit
    (let ((exit 0))
      (dolist (proc (reverse processes))
        (sb-ext:process-wait proc)
        (setf exit (or (sb-ext:process-exit-code proc) 0))
        ;; If last process has output stream, drain it
        (let ((out (sb-ext:process-output proc)))
          (when (and out (streamp out) (open-stream-p out))
            (handler-case
                (loop for line = (read-line out nil nil)
                      while line do (write-line line))
              (error ())))))
      exit)))

(defun execute-pipeline (pipeline-ast)
  "Execute a pipeline AST using OS pipes."
  (let ((commands (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
                      (nshell.domain.parsing:pipeline-node-commands pipeline-ast)
                      (list pipeline-ast))))
    (if (null (cdr commands))
        ;; Single command - execute directly
        (let* ((cmd-node (first commands)))
          (execute-external
           (nshell.domain.parsing:command-node-command cmd-node)
           (nshell.domain.parsing:command-node-args cmd-node)))
        ;; Pipeline - use OS pipes
        (execute-pipeline-commands commands))))

(defun execute-pipeline-use-case (pipeline dispatcher)
  (declare (ignore dispatcher))
  (execute-pipeline pipeline))
