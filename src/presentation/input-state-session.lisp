;;; Session-level state transitions for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun %preserve-completion-session-p (old-state state key-event-type)
  (and (eq (input-state-mode old-state) :insert)
       (eq (input-state-mode state) :insert)
       (not (member key-event-type '(:escape :ctrl-g :ctrl-c)
                    :test #'eq))
       (or (member key-event-type '(:tab :shift-tab) :test #'eq)
           (eq key-event-type :ctrl-l)
           (let ((suggestion (input-state-suggestion state)))
             (and (stringp suggestion)
                  (plusp (length suggestion)))))))

(defun finalize-input-state-transition (old-state new-state key-event)
  (let ((key-event-type (nshell.domain.input:key-event-type key-event)))
    (when (eq key-event-type :ctrl-l)
      (return-from finalize-input-state-transition new-state))
    (let ((state (if (member key-event-type '(:ctrl-y :alt-y) :test #'eq)
                     new-state
                     (copy-input-state-with new-state
                                            :last-yank-start nil
                                            :last-yank-end nil
                                            :last-yank-index nil))))
      (setf state (if (eq key-event-type :alt-dot)
                      state
                      (copy-input-state-with state
                                             :last-argument-start nil
                                             :last-argument-end nil
                                             :last-argument-index nil)))
      (if (%preserve-completion-session-p old-state state key-event-type)
          state
          (clear-completion-session-state state)))))

(defun reduce-input-state (state key-event)
  "Apply KEY-EVENT to INPUT-STATE and return two values.

The first value is a fresh INPUT-STATE. The second value is an OUTPUT-EVENT
keyword for the impure REPL shell to interpret. This function performs no I/O
  and mutates neither STATE nor KEY-EVENT."
  (with-normalized-input-state (state state)
    (multiple-value-bind (new-state output)
        (if (eq (input-state-mode state) :search)
            (reduce-search-input-state state key-event)
            (reduce-insert-input-state state key-event))
      (let ((final-state (finalize-input-state-transition state new-state key-event)))
        (values (record-undo-transition state final-state output key-event)
                output)))))
