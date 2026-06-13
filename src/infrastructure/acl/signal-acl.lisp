(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defvar *shell-pgid* 0)
(defvar *foreground-pgid* 0)
(defvar *terminal-resized* nil)
(defvar *children-changed* nil)
(defvar *sigint-received* nil)

(defun os-signal->domain (os-signal)
  (let ((sig-map `((:sigint . ,sb-unix:sigint)
                   (:sigterm . ,sb-unix:sigterm)
                   (:sigtstp . ,sb-unix:sigtstp)
                   (:sigcont . ,sb-unix:sigcont)
                   (:sigchld . ,sb-unix:sigchld)
                   (:sigwinch . ,sb-unix:sigwinch))))
    (let ((num (cdr (assoc os-signal sig-map))))
      (when num (nshell.domain.signals:make-signal os-signal num)))))

(defun domain-signal->os (domain-signal)
  (nshell.domain.signals:signal-name domain-signal))

(defun shell-sigint-handler (signal info context)
  "Forward SIGINT to the foreground process group without killing the shell."
  (declare (ignore signal info context))
  (when (> *foreground-pgid* 0)
    (sb-posix:kill (- *foreground-pgid*) sb-unix:sigint))
  (setf *sigint-received* t))

(defun shell-sigtstp-handler (signal info context)
  "Restore terminal state, then suspend the shell."
  (declare (ignore signal info context))
  (ignore-errors (nshell.infrastructure.terminal:restore-terminal-mode))
  (sb-sys:enable-interrupt sb-unix:sigtstp :default)
  (sb-posix:kill (sb-posix:getpid) sb-unix:sigtstp))

(defun shell-sigchld-handler (signal info context)
  "Record that child process state changed; reaping is done outside the handler."
  (declare (ignore signal info context))
  (setf *children-changed* t))

(defun shell-sigwinch-handler (signal info context)
  "Record terminal resize events for the main loop."
  (declare (ignore signal info context))
  (setf *terminal-resized* t))

(defun shell-sigcont-handler (signal info context)
  "Re-enable raw mode and reclaim the terminal after continuing."
  (declare (ignore signal info context))
  (ignore-errors (nshell.infrastructure.terminal:enable-raw-mode))
  (ignore-errors (set-foreground-pgroup (sb-posix:getpid))))

(defun install-signal-handlers ()
  "Install shell signal handlers for job-control aware interactive operation."
  (setf *shell-pgid* (sb-posix:getpid))
  (sb-sys:enable-interrupt sb-unix:sigint #'shell-sigint-handler)
  (sb-sys:enable-interrupt sb-unix:sigtstp #'shell-sigtstp-handler)
  (sb-sys:enable-interrupt sb-unix:sigchld #'shell-sigchld-handler)
  (sb-sys:enable-interrupt sb-unix:sigwinch #'shell-sigwinch-handler)
  (sb-sys:enable-interrupt sb-unix:sigcont #'shell-sigcont-handler)
  (sb-sys:enable-interrupt sb-unix:sigttou :ignore)
  (sb-sys:enable-interrupt sb-unix:sigttin :ignore)
  t)
