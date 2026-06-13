(in-package #:nshell.domain.events)

;;; Ensure the event API is externally visible without changing package.lisp.
(export '(domain-event
          domain-event-p
          make-domain-event
          domain-event-type
          domain-event-timestamp
          event-type-p
          make-command-entered-event
          make-command-parsed-event
          make-parse-failed-event
          make-pipeline-started-event
          make-process-created-event
          make-process-exited-event
          make-pipeline-completed-event
          make-job-created-event
          make-job-stopped-event
          make-job-continued-event
          make-job-completed-event
          make-signal-caught-event
          make-command-appended-to-history-event
          make-completion-triggered-event))

;;; Base domain event protocol
(defstruct (domain-event
            (:constructor make-domain-event
                (type &optional (timestamp (get-universal-time)))))
  "A domain event representing something that happened in the system.
TYPE is a keyword identifying the event kind.
TIMESTAMP is a universal time (default: now)."
  (type nil :type keyword :read-only t)
  (timestamp (get-universal-time) :type integer :read-only t))

(defun event-type-p (event expected-type)
  "Check if EVENT has the expected TYPE keyword."
  (and (domain-event-p event)
       (eq (domain-event-type event) expected-type)))

;;; Compatibility aliases for the current Wave 0 package exports.
(defun make-event (type &optional (timestamp (get-universal-time)))
  "Create a domain event of TYPE with optional TIMESTAMP."
  (make-domain-event type timestamp))

(defun event-type (event)
  "Return the event type keyword for EVENT."
  (domain-event-type event))

(defun event-timestamp (event)
  "Return the universal-time timestamp for EVENT."
  (domain-event-timestamp event))

;;; Command-related domain events
(defun make-command-entered-event (text)
  "Create event: user entered a command line TEXT."
  (declare (ignore text))
  (make-domain-event :command-entered))

(defun make-command-parsed-event (ast)
  "Create event: command line was successfully parsed into AST."
  (declare (ignore ast))
  (make-domain-event :command-parsed))

(defun make-parse-failed-event (raw-text error-message)
  "Create event: parsing failed for RAW-TEXT with ERROR-MESSAGE."
  (declare (ignore raw-text error-message))
  (make-domain-event :parse-failed))

(defun make-pipeline-started-event (pipeline job-id)
  "Create event: PIPELINE started with JOB-ID."
  (declare (ignore pipeline job-id))
  (make-domain-event :pipeline-started))

(defun make-process-created-event (job-id pid)
  "Create event: process created with JOB-ID and PID."
  (declare (ignore job-id pid))
  (make-domain-event :process-created))

(defun make-process-exited-event (job-id exit-code)
  "Create event: process with JOB-ID exited with EXIT-CODE."
  (declare (ignore job-id exit-code))
  (make-domain-event :process-exited))

(defun make-pipeline-completed-event (job-id exit-code)
  "Create event: pipeline with JOB-ID completed with EXIT-CODE."
  (declare (ignore job-id exit-code))
  (make-domain-event :pipeline-completed))

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
