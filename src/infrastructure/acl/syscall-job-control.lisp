(in-package #:nshell.infrastructure.acl)

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
              ((plusp pid)
               (push (cons pid status) children))
              (t
               (return (nreverse children)))))
        (sb-posix:syscall-error (condition)
          (if (= (sb-posix:syscall-errno condition) sb-posix:echild)
              (return (nreverse children))
              (error condition)))))))

(defun %decode-wait-status (pid status)
  (cond
    ((or (null pid) (zerop pid))
     (values pid :running nil))
    ((sb-posix:wifstopped status)
     (values pid :stopped (sb-posix:wstopsig status)))
    ((sb-posix:wifexited status)
     (values pid :exited (sb-posix:wexitstatus status)))
    ((sb-posix:wifsignaled status)
     (values pid :signaled (sb-posix:wtermsig status)))
    ((sb-posix:wifcontinued status)
     (values pid :continued nil))
    (t
     (values pid :unknown status))))

(defun wait-job (pid &key nohang untraced continued)
  "Wait for PID or process group PID and return (values child-pid state detail)."
  (declare (ignore continued))
  (let ((flags 0))
    (when nohang
      (setf flags (logior flags sb-posix:wnohang)))
    (when untraced
      (setf flags (logior flags sb-posix:wuntraced)))
    (handler-case
        (multiple-value-bind (child-pid status) (sb-posix:waitpid pid flags)
          (%decode-wait-status child-pid status))
      (sb-posix:syscall-error (condition)
        (if (= (sb-posix:syscall-errno condition) sb-posix:echild)
            (values nil :no-child nil)
            (error condition))))))
