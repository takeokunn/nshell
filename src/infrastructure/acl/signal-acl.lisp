(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun os-signal->domain (os-signal)
  (let ((sig-map '((:sigint . 2) (:sigterm . 15) (:sigtstp . 20) (:sigcont . 18) (:sigchld . 17))))
    (let ((num (cdr (assoc os-signal sig-map))))
      (when num (nshell.domain.signals:make-signal os-signal num)))))

(defun domain-signal->os (domain-signal)
  (nshell.domain.signals:signal-name domain-signal))

(defun install-signal-handlers ()
  (sb-sys:enable-interrupt sb-posix:sigint
    (lambda (sig info ctx)
      (declare (ignore sig info ctx))
      (format t "~%Caught SIGINT~%")
      (sb-ext:quit))))
