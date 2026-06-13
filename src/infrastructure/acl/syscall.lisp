(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(sb-alien:define-alien-routine ("tcsetpgrp" %tcsetpgrp) sb-alien:int
  (fd sb-alien:int)
  (pgid sb-alien:int))

(sb-alien:define-alien-routine ("tcgetpgrp" %tcgetpgrp) sb-alien:int
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("ioctl" %ioctl) sb-alien:int
  (fd sb-alien:int)
  (request sb-alien:unsigned-long)
  (arg sb-sys:system-area-pointer))

(defconstant +tiocgwinsz+
  #+darwin #x40087468
  #+linux #x5413
  #-(or darwin linux) 0)

(defun spawn-async (cmd args)
  "Spawn CMD with ARGS asynchronously. Returns the SBCL process object, or NIL on error.
   Sets process group for proper job control. The caller is responsible for process-wait."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :input *standard-input*
                    :output :stream
                    :error :output
                    :wait nil
                    :search t
                    :environment (%get-environment))))
        (when proc
          ;; Set process group for job control
          (let ((pid (sb-ext:process-pid proc)))
            (when (plusp pid)
              (handler-case (nshell.infrastructure.acl:set-process-group pid pid)
                (error ()))))
          proc))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      nil)))
(defvar *exported-environment* nil
  "List of \"KEY=VALUE\" strings for exported environment variables.
   Set by the REPL from the current shell environment.")

(defun %get-environment ()
  "Return the exported environment list for subprocess execution."
  (or *exported-environment* nil))

(defun run-external (cmd args)
  "Execute CMD with ARGS synchronously, printing output. Returns exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :input *standard-input*
                    :output :stream :error :output
                    :wait t :search t
                    :environment (%get-environment))))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out (loop for line = (read-line out nil nil) while line do (write-line line))))
          (or (sb-ext:process-exit-code proc) 0)))
    (error (err) (format *error-output* "nshell: ~a: ~a~%" cmd err) 1)))

(defun spawn-pipeline (commands)
  "Execute COMMANDS connected by OS-level pipes using sb-posix:pipe.
   Each command runs as a subprocess; stdout of stage N connects to stdin of stage N+1.
   Returns exit code of the last command."
  (let* ((n (length commands))
         (pipes nil)
         (procs nil))
    ;; Create pipe pairs for each inter-stage connection
    (dotimes (i (1- n))
      (push (multiple-value-list (sb-posix:pipe)) pipes))
    (setf pipes (nreverse pipes))
    ;; Spawn each process, connecting pipes
    (loop for i from 0 below n
          for cmd-node in commands
          for cmd = (nshell.domain.parsing:command-node-command cmd-node)
          for args = (mapcar #'nshell.domain.parsing:arg-value
                             (nshell.domain.parsing:command-node-args cmd-node))
          for prev-pipe = (if (> i 0) (nth (1- i) pipes) nil)
          for next-pipe = (if (< i (1- n)) (nth i pipes) nil)
          do (handler-case
                 (let ((proc (sb-ext:run-program cmd args
                              :input (if prev-pipe
                                         (sb-sys:make-fd-stream (first prev-pipe)
                                                                :input t
                                                                :buffering :line)
                                         t)
                              :output (if next-pipe
                                          (sb-sys:make-fd-stream (second next-pipe)
                                                                 :output t
                                                                 :buffering :line)
                                          :stream)
                              :error :output
                              :wait nil
                              :search t)))
                   (push proc procs)
                   ;; Close our copy of prev-pipe read end (child has it)
                   (when prev-pipe
                     (sb-posix:close (first prev-pipe)))
                   ;; Close our copy of next-pipe write end (child output goes to it)
                   (when next-pipe
                     (sb-posix:close (second next-pipe))))
               (error (err)
                 (format *error-output* "nshell: ~a: ~a~%" cmd err))))
    ;; Read output from last process
    (let ((last-proc (first procs)))
      (when (and last-proc (sb-ext:process-output last-proc))
        (handler-case
            (loop for line = (read-line (sb-ext:process-output last-proc) nil nil)
                  while line do (write-line line))
          (error ()))))
    ;; Wait for all processes and return last exit code
    (let ((exit 0))
      (dolist (proc (reverse procs))
        (sb-ext:process-wait proc)
        (setf exit (or (sb-ext:process-exit-code proc) 0)))
      ;; Close any remaining pipe fd that may still be open
      (dolist (p pipes)
        (ignore-errors (sb-posix:close (first p)))
        (ignore-errors (sb-posix:close (second p))))
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

(defun set-process-group (pid pgid)
  "Set PID's process group to PGID."
  (sb-posix:setpgid pid pgid))

(defun set-foreground-pgroup (pgid)
  "Make PGID the foreground process group for the controlling terminal."
  (let ((result (%tcsetpgrp 0 pgid)))
    (when (minusp result)
      (error "tcsetpgrp failed with errno ~d" (sb-unix::get-errno)))
    result))

(defun get-foreground-pgroup ()
  "Return the foreground process group of the controlling terminal."
  (let ((result (%tcgetpgrp 0)))
    (when (minusp result)
      (error "tcgetpgrp failed with errno ~d" (sb-unix::get-errno)))
    result))

(defun make-process-group-leader ()
  "Create a new session and make this process its leader."
  (sb-posix:setsid))

(defun reap-children ()
  "Reap all changed child processes without blocking. Returns a list of (pid . status)."
  (let ((children nil))
    (loop
      (handler-case
          (multiple-value-bind (pid status) (sb-posix:waitpid -1 sb-posix:wnohang)
            (cond
              ((plusp pid) (push (cons pid status) children))
              (t (return (nreverse children)))))
        (sb-posix:syscall-error (condition)
          (if (= (sb-posix:syscall-errno condition) sb-posix:echild)
              (return (nreverse children))
              (error condition)))))))

(defun get-terminal-size ()
  "Return terminal size as (values rows cols)."
  (sb-alien:with-alien ((winsize (array sb-alien:unsigned-short 4)))
    (let ((result (%ioctl 0 +tiocgwinsz+ (sb-alien:alien-sap winsize))))
      (when (minusp result)
        (error "ioctl(TIOCGWINSZ) failed with errno ~d" (sb-unix::get-errno)))
      (values (sb-alien:deref winsize 0)
              (sb-alien:deref winsize 1)))))
