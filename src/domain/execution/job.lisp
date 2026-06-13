(in-package #:nshell.domain.execution)

;;; Job entity (aggregate root)
;;; Job states: :created, :running, :stopped, :background, :completed

(defstruct (job
            (:constructor %make-job (id-int pipeline-pipe))
            (:conc-name job-))
  "A job representing an executing pipeline.
ID is a unique integer identifier.
PIPELINE is the pipeline being executed.
STATE is one of: :created, :running, :stopped, :background, :completed.
PGID is the process group ID (set by infrastructure).
EXIT-CODE is set when completed."
  (id-int 0 :type integer :read-only t)
  (pipeline-pipe nil :type (or null pipeline) :read-only t)
  (state-kw :created :type keyword)
  (pgid 0 :type integer)
  (exit-code nil :type (or null integer)))

(defun make-job (id pipeline)
  "Create a job for PIPELINE with unique integer ID."
  (%make-job id pipeline))

(defun job-id (j)
  "Return the job's unique ID."
  (job-id-int j))

(defun job-state (j)
  "Return the current job state keyword."
  (job-state-kw j))

(defun job-pipeline (j)
  "Return the job's pipeline."
  (job-pipeline-pipe j))

(defun job-state-valid-p (state)
  "Check if STATE is a valid job state keyword."
  (not (null (member state '(:created :running :stopped :background :completed)))))

(defun job-state-transition (job new-state)
  "Return JOB with updated state.
Returns the same struct if state is unchanged."
  (unless (job-state-valid-p new-state)
    (error "Invalid job state: ~s" new-state))
  (unless (eq (job-state-kw job) new-state)
    (setf (job-state-kw job) new-state))
  job)

(defun job-running-p (job)
  "True if job is in :running state."
  (eq (job-state-kw job) :running))

(defun job-stopped-p (job)
  "True if job is in :stopped state."
  (eq (job-state-kw job) :stopped))

(defun job-completed-p (job)
  "True if job is in :completed state."
  (eq (job-state-kw job) :completed))

(export '(job-pgid
          job-exit-code
          job-state-transition
          job-running-p
          job-stopped-p
          job-completed-p))
