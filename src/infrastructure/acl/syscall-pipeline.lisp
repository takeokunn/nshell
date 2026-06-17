(in-package #:nshell.infrastructure.acl)

(defun %pipeline-stage-streams (stage-redirects prev-pipe next-pipe redirect-streams
                                &key (default-output :stream))
  (let ((input-pipe-stream nil)
        (output-pipe-stream nil))
    (multiple-value-bind (output-target output-mode)
        (%redirect-output-spec stage-redirects)
      (let ((input (cond
                     ((%redirect-target stage-redirects :<)
                      (let ((stream (open (%redirect-target stage-redirects :<)
                                          :direction :input
                                          :if-does-not-exist :error)))
                        (push stream redirect-streams)
                        stream))
                     (prev-pipe
                      (setf input-pipe-stream
                            (sb-sys:make-fd-stream (first prev-pipe)
                                                   :input t
                                                   :buffering :line)))
                     (t t)))
            (output (cond
                      (output-target
                       (let ((stream (open output-target
                                           :direction :output
                                           :if-exists output-mode
                                           :if-does-not-exist :create)))
                         (push stream redirect-streams)
                         stream))
                      (next-pipe
                       (setf output-pipe-stream
                             (sb-sys:make-fd-stream (second next-pipe)
                                                    :output t
                                                    :buffering :line)))
                      (t default-output))))
        (values input output input-pipe-stream output-pipe-stream redirect-streams)))))

(defun %spawn-pipeline-stage (cmd-node stage-redirects prev-pipe next-pipe redirect-streams
                              &key (default-output :stream))
  (multiple-value-bind (input output input-pipe-stream output-pipe-stream redirect-streams)
      (%pipeline-stage-streams stage-redirects prev-pipe next-pipe redirect-streams
                               :default-output default-output)
    (let* ((cmd (nshell.domain.parsing:command-node-command cmd-node))
           (args (mapcar #'nshell.domain.parsing:arg-value
                         (nshell.domain.parsing:command-node-args cmd-node)))
           (proc (sb-ext:run-program cmd args
                   :input input
                   :output output
                   :error :output
                   :wait nil
                   :search t
                   :environment (%get-environment))))
      (when input-pipe-stream
        (ignore-errors (close input-pipe-stream)))
      (when output-pipe-stream
        (ignore-errors (close output-pipe-stream)))
      (values proc redirect-streams))))

(defun %drain-process-output (proc)
  (when (and proc (sb-ext:process-output proc))
    (handler-case
        (loop for line = (read-line (sb-ext:process-output proc) nil nil)
              while line
              do (write-line line))
      (error ()))))

(defun %wait-pipeline-processes (procs)
  (let ((exit 0))
    (dolist (proc (reverse procs))
      (sb-ext:process-wait proc)
      (setf exit (or (sb-ext:process-exit-code proc) 0)))
    exit))

(defun %close-pipeline-fds (pipes)
  (dolist (pipe pipes)
    (ignore-errors (sb-posix:close (first pipe)))
    (ignore-errors (sb-posix:close (second pipe)))))

(defun spawn-pipeline (commands &key redirects)
  "Execute COMMANDS connected by OS-level pipes and return the last exit code."
  (let* ((count (length commands))
         (redirects (or redirects (make-list count :initial-element nil)))
         (pipes (loop repeat (max 0 (1- count))
                      collect (multiple-value-list (sb-posix:pipe))))
         (procs nil)
         (redirect-streams nil)
         (spawn-error-code nil))
    (unwind-protect
         (progn
           (loop for index from 0 below count
                 for cmd-node in commands
                 for stage-redirects = (nth index redirects)
                 for prev-pipe = (and (plusp index) (nth (1- index) pipes))
                 for next-pipe = (and (< index (1- count)) (nth index pipes))
                 while (null spawn-error-code)
                 do (handler-case
                        (multiple-value-bind (proc updated-streams)
                            (%spawn-pipeline-stage cmd-node
                                                   stage-redirects
                                                   prev-pipe
                                                   next-pipe
                                                   redirect-streams)
                          (setf redirect-streams updated-streams)
                          (push proc procs))
                      (error (err)
                        (setf spawn-error-code 127)
                        (format *error-output* "nshell: ~a: ~a~%"
                                (nshell.domain.parsing:command-node-command cmd-node)
                                err))))
           (%drain-process-output (first procs))
           (or spawn-error-code
               (%wait-pipeline-processes procs)))
      (dolist (stream redirect-streams)
        (ignore-errors (close stream)))
      (%close-pipeline-fds pipes))))

(defun spawn-pipeline-async (commands &key redirects)
  "Execute COMMANDS connected by OS-level pipes asynchronously."
  (let* ((count (length commands))
         (redirects (or redirects (make-list count :initial-element nil)))
         (pipes (loop repeat (max 0 (1- count))
                      collect (multiple-value-list (sb-posix:pipe))))
         (procs nil)
         (pgid nil)
         (redirect-streams nil))
    (unwind-protect
         (progn
           (loop for index from 0 below count
                 for cmd-node in commands
                 for stage-redirects = (nth index redirects)
                 for prev-pipe = (and (plusp index) (nth (1- index) pipes))
                 for next-pipe = (and (< index (1- count)) (nth index pipes))
                 do (handler-case
                        (multiple-value-bind (proc updated-streams)
                            (%spawn-pipeline-stage cmd-node
                                                   stage-redirects
                                                   prev-pipe
                                                   next-pipe
                                                   redirect-streams
                                                   :default-output t)
                          (setf redirect-streams updated-streams)
                          (when proc
                            (let ((pid (sb-ext:process-pid proc)))
                              (when (plusp pid)
                                (unless pgid
                                  (setf pgid pid))
                                (handler-case (set-process-group pid pgid)
                                  (error ()))))
                            (push proc procs)))
                      (error (err)
                        (format *error-output* "nshell: ~a: ~a~%"
                                (nshell.domain.parsing:command-node-command cmd-node)
                                err))))
           (nreverse procs))
      (dolist (stream redirect-streams)
        (ignore-errors (close stream)))
      (%close-pipeline-fds pipes))))
