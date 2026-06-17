;;; Session-level state transitions for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defmacro define-key-event-membership-predicate (name event-types)
  `(defun ,name (key-event)
     (not (null (member (key-event-type key-event) ',event-types :test #'eq)))))

(defmacro define-key-event-eq-predicate (name event-type)
  `(defun ,name (key-event)
     (eq (key-event-type key-event) ,event-type)))

(define-key-event-membership-predicate key-preserves-yank-pop-p (:ctrl-y :alt-y))

(define-key-event-membership-predicate key-preserves-completion-session-p
    (:tab :shift-tab))

(define-key-event-eq-predicate key-preserves-last-argument-session-p :alt-dot)

(define-key-event-membership-predicate key-cancels-completion-session-p
    (:escape :ctrl-g :ctrl-c))

(define-key-event-eq-predicate key-preserves-clear-screen-session-p :ctrl-l)

(defun completion-session-preserved-p (old-state new-state key-event)
  (and (eq (input-state-mode old-state) :insert)
       (eq (input-state-mode new-state) :insert)
       (not (key-cancels-completion-session-p key-event))
       (or (key-preserves-completion-session-p key-event)
           (key-preserves-clear-screen-session-p key-event)
           (let ((suggestion (input-state-suggestion new-state)))
             (and (stringp suggestion)
                  (plusp (length suggestion)))))))

(defun clear-yank-pop-state (state)
  (copy-input-state-with state
                         :last-yank-start nil
                         :last-yank-end nil
                         :last-yank-index nil))

(defun clear-last-argument-state (state)
  (copy-input-state-with state
                         :last-argument-start nil
                         :last-argument-end nil
                         :last-argument-index nil))

(defun finalize-input-state-transition (old-state new-state key-event)
  (when (eq (key-event-type key-event) :ctrl-l)
    (return-from finalize-input-state-transition new-state))
  (let* ((state (if (key-preserves-yank-pop-p key-event)
                    new-state
                    (clear-yank-pop-state new-state)))
         (state (if (key-preserves-last-argument-session-p key-event)
                    state
                    (clear-last-argument-state state))))
    (if (completion-session-preserved-p old-state state key-event)
        state
        (clear-completion-session-state state))))

(defun reduce-input-state (state key-event)
  "Apply KEY-EVENT to INPUT-STATE and return two values.

The first value is a fresh INPUT-STATE. The second value is an OUTPUT-EVENT
keyword for the impure REPL shell to interpret. This function performs no I/O
  and mutates neither STATE nor KEY-EVENT."
  (let ((state (normalize-input-state state)))
    (multiple-value-bind (new-state output)
        (if (eq (input-state-mode state) :search)
            (reduce-search-input-state state key-event)
            (reduce-insert-input-state state key-event))
      (let ((final-state (finalize-input-state-transition state new-state key-event)))
        (values (record-undo-transition state final-state output key-event)
                output)))))
