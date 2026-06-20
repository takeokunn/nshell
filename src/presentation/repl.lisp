;;; nshell REPL - CPS-based interactive shell loop
;;; fish-inspired UX with trampoline-driven continuations
(in-package #:nshell.presentation)

(defun read-key-cont ()
  (let ((event (nshell.infrastructure.terminal:read-key-event)))
    (if event
      (lambda ()
          (multiple-value-bind (new-state output-event)
              (reduce-input-state *input-state* event)
            (setf *input-state* new-state)
            (process-output-event output-event)))
        (progn
          (setf *running* nil)
          nil))))

;; REPL Entry
(defun run-repl ()
  (initialize-repl-state)
  (install-interactive-terminal)
  (unwind-protect
      (trampoline (lambda () (render-prompt-cont)))
    (restore-interactive-terminal)
    (format t "Goodbye!~%")))
