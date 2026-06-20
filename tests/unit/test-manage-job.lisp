(in-package #:nshell/test)

(def-suite manage-job-service-tests
  :description "Application job-management service tests"
  :in nshell-tests)

(in-suite manage-job-service-tests)

(test jobs-prints-current-monitor-entries
  "JOBS formats the process-wide monitor entries and returns them."
  (let* ((monitor (nshell.domain.job-control:make-job-monitor))
         (job (make-test-job 0 "echo" :args '("hello"))))
    (nshell.domain.job-control:monitor-add-job monitor job)
    (let ((nshell.application:*job-monitor* monitor))
      (let ((output (capture-standard-output
                      (let ((entries (nshell.application:jobs)))
                        (is (= 1 (length entries)))))))
        (is (search "[0]" output))
        (is (search "Created" output))
        (is (search "echo hello" output))))))

(test bg-marks-job-as-background-and-publishes-continuation
  "BG updates the job state without requiring terminal control when PGID is zero."
  (let* ((monitor (nshell.domain.job-control:make-job-monitor))
         (dispatcher (nshell.application:make-event-dispatcher))
         (job (make-test-job 0 "sleep" :args '("10")))
         (job-id (nshell.domain.job-control:monitor-add-job monitor job))
         (continued nil)
         (nshell.application:*job-monitor* monitor))
    (with-event-capture (continued dispatcher :job-continued)
        (nshell.domain.events:domain-event-type event)
      (is (eq job (nshell.application:bg job-id dispatcher)))
      (is (eq :background (nshell.domain.execution:job-state job)))
      (is (nshell.domain.execution:job-background-p job))
      (is (null (nshell.application:drain-events dispatcher)))
      (is (equal '(:job-continued) (nreverse continued))))))

(test disown-removes-job-from-monitor
  "DISOWN removes a tracked job and returns true for an existing id."
  (let* ((monitor (nshell.domain.job-control:make-job-monitor))
         (job (make-test-job 0 "sleep" :args '("10")))
         (job-id (nshell.domain.job-control:monitor-add-job monitor job))
         (nshell.application:*job-monitor* monitor))
    (is (nshell.application:disown job-id))
    (is (null (nshell.domain.job-control:monitor-find-job monitor job-id)))))

(test missing-job-commands-return-nil-and-report
  "BG/FG report missing jobs instead of signaling application errors."
  (let ((nshell.application:*job-monitor* (nshell.domain.job-control:make-job-monitor)))
    (let ((bg-output (with-captured-stdout (output)
                       (is (null (nshell.application:bg 42)))))
          (fg-output (with-captured-stdout (output)
                       (is (null (nshell.application:fg 42))))))
      (is (search "bg: no such job: 42" bg-output))
      (is (search "fg: no such job: 42" fg-output)))))
