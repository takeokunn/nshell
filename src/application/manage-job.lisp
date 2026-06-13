(in-package #:nshell.application)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defvar *job-monitor* (nshell.domain.job-control:make-job-monitor))
(defvar *shell-pgid* (sb-posix:getpid))
(defvar *foreground-job-pgid* nil)

(defun %job-command-string (job)
  (or (and (> (length (nshell.domain.execution:job-command-line job)) 0)
           (nshell.domain.execution:job-command-line job))
      (let ((pipeline (nshell.domain.execution:job-pipeline job)))
        (if pipeline
            (format nil "~{~{~a~^ ~}~^ | ~}"
                    (mapcar #'nshell.domain.execution:command-to-list
                            (nshell.domain.execution:pipeline-commands pipeline)))
            ""))))

(defun %status-label (job)
  (case (nshell.domain.execution:job-state job)
    (:running "Running")
    (:background "Running")
    (:stopped "Stopped")
    ((:completed :done) "Done")
    (:created "Created")
    (otherwise "Unknown")))

(defun %with-terminal-foreground-pgroup (pgid thunk)
  (let ((previous (ignore-errors (nshell.infrastructure.acl:get-foreground-pgroup))))
    (unwind-protect
         (progn
           (ignore-errors (nshell.infrastructure.acl:set-foreground-pgroup pgid))
           (funcall thunk))
      (ignore-errors
        (nshell.infrastructure.acl:set-foreground-pgroup (or previous *shell-pgid*))))))

(defun %wait-job-pgid (job)
  (let ((pgid (nshell.domain.execution:job-pgid job))
        (status-code 0))
    (loop
      (handler-case
          (multiple-value-bind (pid status)
              (sb-posix:waitpid (- pgid) sb-posix:wuntraced)
            (declare (ignore pid))
            (cond
              ((sb-posix:wifstopped status)
               (nshell.domain.execution:job-state-transition job :stopped)
               (return job))
              ((sb-posix:wifexited status)
               (setf status-code (sb-posix:wexitstatus status)
                     (nshell.domain.execution:job-exit-code job) status-code)
               (nshell.domain.execution:job-state-transition job :completed)
               (return job))
              ((sb-posix:wifsignaled status)
               (setf status-code (+ 128 (sb-posix:wtermsig status))
                     (nshell.domain.execution:job-exit-code job) status-code)
               (nshell.domain.execution:job-state-transition job :completed)
               (return job))))
        (sb-posix:syscall-error (condition)
          (if (= (sb-posix:syscall-errno condition) sb-posix:echild)
              (progn
                (nshell.domain.execution:job-state-transition job :completed)
                (return job))
              (error condition)))))))

(defun %require-job (job-id command)
  (or (nshell.domain.job-control:monitor-find-job *job-monitor* job-id)
      (progn
        (format t "~a: no such job: ~a~%" command job-id)
        nil)))

(defun fg (job-id)
  "Move JOB-ID to the foreground, wait for it, then restore the shell PGID."
  (let ((job (%require-job job-id "fg")))
    (when job
      (let ((pgid (nshell.domain.execution:job-pgid job)))
        (when (plusp pgid)
          (setf *foreground-job-pgid* pgid
                (nshell.domain.execution:job-background-p job) nil)
          (sb-posix:kill (- pgid) sb-unix:sigcont)
          (nshell.domain.execution:job-state-transition job :running)
          (%with-terminal-foreground-pgroup
           pgid
           (lambda () (%wait-job-pgid job)))
          (setf *foreground-job-pgid* nil))
        job))))

(defun bg (job-id)
  "Continue JOB-ID in the background."
  (let ((job (%require-job job-id "bg")))
    (when job
      (let ((pgid (nshell.domain.execution:job-pgid job)))
        (when (plusp pgid)
          (sb-posix:kill (- pgid) sb-unix:sigcont))
        (setf (nshell.domain.execution:job-background-p job) t)
        (nshell.domain.execution:job-state-transition job :background)
        job))))

(defun jobs ()
  "Print and return current jobs."
  (let ((entries (nshell.domain.job-control:monitor-entries *job-monitor*)))
    (dolist (entry entries)
      (let ((jid (car entry))
            (job (cdr entry)))
        (format t "[~d] ~a ~a~%"
                jid
                (%status-label job)
                (%job-command-string job))))
    entries))

(defun disown (job-id)
  "Remove JOB-ID from the job monitor."
  (nshell.domain.job-control:monitor-remove-job *job-monitor* job-id))

(defun interrupt-foreground ()
  "Send SIGINT to the foreground job process group."
  (let ((pgid (or *foreground-job-pgid*
                  (ignore-errors (nshell.infrastructure.acl:get-foreground-pgroup)))))
    (when (and pgid (plusp pgid))
      (sb-posix:kill (- pgid) sb-unix:sigint))))

(defun suspend-foreground ()
  "Send SIGTSTP to the foreground job process group."
  (let ((pgid (or *foreground-job-pgid*
                  (ignore-errors (nshell.infrastructure.acl:get-foreground-pgroup)))))
    (when (and pgid (plusp pgid))
      (sb-posix:kill (- pgid) sb-unix:sigtstp))))
