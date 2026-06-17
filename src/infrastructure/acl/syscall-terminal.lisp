(in-package #:nshell.infrastructure.acl)

(defun get-terminal-size ()
  "Return terminal size as (values rows cols)."
  (sb-alien:with-alien ((winsize (array sb-alien:unsigned-short 4)))
    (let ((result (%ioctl 0 +tiocgwinsz+ (sb-alien:alien-sap winsize))))
      (when (minusp result)
        (error "ioctl(TIOCGWINSZ) failed with errno ~d" (sb-unix::get-errno)))
      (values (sb-alien:deref winsize 0)
              (sb-alien:deref winsize 1)))))
