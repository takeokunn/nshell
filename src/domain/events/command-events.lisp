(in-package #:nshell.domain.events)

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
