;;; REPL output-event dispatcher
(in-package #:nshell.presentation)

(defmacro define-key-output-dispatcher (name event-var &body clauses)
  `(defun ,name (,event-var)
     (case ,event-var
       ,@clauses)))

(define-key-output-dispatcher process-output-event output-event
  (:execute
   (%process-execute-output-event))
  (:quit
   (setf *running* nil)
   (done))
  (:complete
   (%process-complete-output-event))
  (:suggest-update
   (%process-suggest-update-output-event))
  ((:search-start :search-update)
   (%process-history-search-output-event))
  (:history-prev
   (%process-history-prev-output-event))
  (:history-next
   (%process-history-next-output-event))
  (:clear-screen
   (%process-clear-screen-output-event))
  (:insert-last-argument
   (%process-insert-last-argument-output-event))
   (:redraw
    (%process-redraw-output-event))
   (t
   (with-cleared-rendered-completions-and-prompt-cont)))

(defun process-key-cont (event)
  (multiple-value-bind (new-state output-event)
      (reduce-input-state *input-state* event)
    (setf *input-state* new-state)
    (process-output-event output-event)))
