(in-package #:nshell.domain.events)

;;; Job-related domain events
(defun make-job-created-event (job-id command pgid)
  "Create event: job JOB-ID created for COMMAND with process group PGID."
  (declare (ignore job-id command pgid))
  (make-domain-event :job-created))

(defun make-job-stopped-event (job-id signal)
  "Create event: job JOB-ID stopped by SIGNAL."
  (declare (ignore job-id signal))
  (make-domain-event :job-stopped))

(defun make-job-continued-event (job-id)
  "Create event: job JOB-ID continued."
  (declare (ignore job-id))
  (make-domain-event :job-continued))

(defun make-job-completed-event (job-id exit-code)
  "Create event: job JOB-ID completed with EXIT-CODE."
  (declare (ignore job-id exit-code))
  (make-domain-event :job-completed))

(defun make-signal-caught-event (signal)
  "Create event: a signal was caught by the shell."
  (declare (ignore signal))
  (make-domain-event :signal-caught))

(defun make-command-appended-to-history-event (entry)
  "Create event: command ENTRY was appended to history."
  (declare (ignore entry))
  (make-domain-event :command-appended-to-history))

(defun make-completion-triggered-event (prefix)
  "Create event: tab completion triggered for PREFIX."
  (declare (ignore prefix))
  (make-domain-event :completion-triggered))
