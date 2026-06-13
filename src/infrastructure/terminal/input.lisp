(in-package #:nshell.infrastructure.terminal)

(defstruct (key-event (:constructor make-key-event (type &optional char)))
  (type :char :type keyword :read-only t)
  (char nil :type (or null character) :read-only t))

(defun read-key-event ()
  (let ((ch (read-char *standard-input* nil nil)))
    (when ch (make-key-event :char ch))))

;; key-event-type and key-event-char are auto-generated struct accessors
