;;; nshell REPL - CPS-based interactive shell loop
;;; fish-inspired UX with trampoline-driven continuations
(in-package #:nshell.presentation)

(defun read-key-cont ()
  (let ((event (nshell.infrastructure.terminal:read-key-event)))
    (if event
        (lambda () (process-key-cont event))
        (progn
          (setf *running* nil)
          (done)))))

;; REPL Entry
(defun run-repl ()
  (initialize-repl-state)
  (install-interactive-terminal)
  (unwind-protect
      (trampoline (render-prompt-continuation))
    (restore-interactive-terminal)
    (format t "Goodbye!~%")))
