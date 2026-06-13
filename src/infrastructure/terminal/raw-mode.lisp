(in-package #:nshell.infrastructure.terminal)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defvar *saved-termios* nil)

(defun enable-raw-mode ()
  (let ((termios (sb-posix:tcgetattr 0)))
    (setf *saved-termios* termios)
    (let ((raw (sb-posix:tcgetattr 0)))
      (setf (sb-posix:termios-lflag raw)
            (logand (sb-posix:termios-lflag raw)
                    (lognot (logior sb-posix:icanon sb-posix:echo sb-posix:isig))))
      (sb-posix:tcsetattr 0 sb-posix:tcsadrain raw))))

(defun restore-terminal-mode ()
  (when *saved-termios*
    (sb-posix:tcsetattr 0 sb-posix:tcsadrain *saved-termios*)))
