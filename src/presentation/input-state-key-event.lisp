;;; Key-event adapter helpers for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun key-event-type (event)
  "Return EVENT's key type."
  (nshell.domain.input:key-event-type event))

(defun key-event-char (event)
  "Return EVENT's character payload, if any."
  (nshell.domain.input:key-event-char event))

(defun key-event-number (event)
  "Return EVENT's numeric payload, if any."
  (nshell.domain.input:key-event-number event))

(defun key-event-data (event)
  "Return EVENT's structured payload, if any."
  (nshell.domain.input:key-event-data event))
