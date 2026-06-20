(in-package #:nshell.presentation)

;; Trampoline
(defun trampoline (thunk)
  (loop for kont = (funcall thunk) then (funcall kont) while kont))

;; REPL State
(defvar *running* nil)
(defvar *last-exit-code* 0)
(defvar *last-command-duration-ms* nil)
(defvar *history* nil)
(defvar *config* nil)
(defvar *kb* nil)
(defvar *input-state* nil)
(defvar *completion-rendered-lines* 0)
(defvar *prompt-rendered-lines* 0)
(defvar *prompt-rendered-cursor-row* 0)
(defvar *environment* nil)
(defvar *aliases* (make-hash-table :test #'equal))
(defvar *abbreviations* (make-hash-table :test #'equal))
(defvar *functions* (make-hash-table :test #'equal))
(defvar *function-sources* (make-hash-table :test #'equal))
(defvar *proc-registry* (make-hash-table :test #'eql)
  "Maps job-id -> SBCL process object or process list for status checking.")
