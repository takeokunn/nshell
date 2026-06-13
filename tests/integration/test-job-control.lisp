(in-package #:nshell/test)

(def-suite job-control-integration-tests
  :description "Job control integration tests"
  :in nshell-tests)

(in-suite job-control-integration-tests)

(defun %test-job (id command &optional args)
  (let* ((cmd (nshell.domain.execution:make-command command args))
         (pipe (nshell.domain.execution:make-pipeline cmd))
         (job (nshell.domain.execution:make-job id pipe)))
    (setf (nshell.domain.execution:job-command-line job)
          (format nil "~{~a~^ ~}" (nshell.domain.execution:command-to-list cmd)))
    job))

(test job-creation-assigns-unique-ids
  (let* ((monitor (nshell.domain.job-control:make-job-monitor))
         (job1 (%test-job 0 "sleep" '("1")))
         (job2 (%test-job 1 "sleep" '("2")))
         (id1 (nshell.domain.job-control:monitor-add-job monitor job1))
         (id2 (nshell.domain.job-control:monitor-add-job monitor job2)))
    (is (not (= id1 id2)))
    (is (= 0 id1))
    (is (= 1 id2))))

(test job-state-transitions-created-running-stopped-completed
  (let* ((monitor (nshell.domain.job-control:make-job-monitor))
         (job (%test-job 0 "sleep" '("1")))
         (id (nshell.domain.job-control:monitor-add-job monitor job)))
    (is (eq :created (nshell.domain.execution:job-state job)))
    (nshell.domain.job-control:monitor-update monitor id :running)
    (is (eq :running (nshell.domain.execution:job-state job)))
    (nshell.domain.job-control:monitor-update monitor id :stopped)
    (is (eq :stopped (nshell.domain.execution:job-state job)))
    (nshell.domain.job-control:monitor-update monitor id :completed 0)
    (is (eq :completed (nshell.domain.execution:job-state job)))
    (is (= 0 (nshell.domain.execution:job-exit-code job)))))

(test jobs-returns-current-job-list
  (let* ((monitor (nshell.domain.job-control:make-job-monitor))
         (job (%test-job 0 "echo" '("hello"))))
    (nshell.domain.job-control:monitor-add-job monitor job)
    (let ((returned (nshell.domain.job-control:monitor-jobs monitor)))
      (is (= 1 (length returned)))
      (is (search "echo hello"
                  (nshell.domain.execution:job-command-line (first returned)))))))

(test reap-children-cleans-up-zombies
  "Verify that process-wait properly cleans up child processes."
  (let ((proc (sb-ext:run-program "true" nil :wait nil :search t)))
    ;; Wait for the process via SBCL's process-wait
    (sb-ext:process-wait proc)
    (let ((pid (sb-ext:process-pid proc)))
      (is (integerp pid))
      (is (not (sb-ext:process-alive-p proc))
          "Process should be dead after process-wait"))))
