(in-package #:nshell/test)

(in-suite repl-tests)

(test repl-extract-redirects-preserves-dangling-operator
  "Malformed redirect argument lists should not make redirect extraction loop forever."
  (multiple-value-bind (clean redirects)
      (nshell.presentation::extract-redirects '("echo" ">"))
    (is (equal '("echo" ">") clean))
    (is (null redirects)))
  (multiple-value-bind (clean redirects)
      (nshell.presentation::extract-redirects '("echo" ">" "out.txt"))
    (is (equal '("echo") clean))
    (is (equal '((:> . "out.txt")) redirects))))

(test repl-background-command-applies-redirections
  "Background commands should apply redirects before spawning the process."
  (with-repl-test-state
      (let* ((output (merge-pathnames
                      (format nil "nshell-bg-redirect-~d.txt"
                              (get-internal-real-time))
                      (uiop:temporary-directory)))
             (ast (nshell.domain.parsing::make-sequence-node
                   (list (nshell.domain.parsing::make-command-node
                          "printf"
                          (list (cons "%s" t) "bg" ">" (namestring output))))
                   '(:amp)))
             (nshell.application:*job-monitor*
               (nshell.domain.job-control:make-job-monitor)))
      (unwind-protect
           (progn
             (with-output-to-string (*standard-output*)
               (nshell.presentation::execute-ast ast))
             (loop repeat 50
                   until (and (probe-file output)
                              (string= "bg" (uiop:read-file-string output)))
                   do (sleep 0.02))
             (is (probe-file output))
             (is (string= "bg" (uiop:read-file-string output))))
        (ignore-errors
          (when (probe-file output)
            (delete-file output)))))))

(test repl-background-pipeline-registers-processes-and-applies-redirections
  "Background pipelines should spawn every stage and keep their redirects."
  (with-repl-test-state
      (let* ((output (merge-pathnames
                      (format nil "nshell-bg-pipeline-~d.txt"
                              (get-internal-real-time))
                      (uiop:temporary-directory)))
             (pipeline (nshell.domain.parsing:make-pipeline-node
                        (list (nshell.domain.parsing:make-command-node
                               "printf"
                               (list (cons "%s" t) "bg-pipe"))
                              (nshell.domain.parsing:make-command-node
                               "cat"
                               (list ">" (namestring output))))))
             (ast (nshell.domain.parsing::make-sequence-node
                   (list pipeline)
                   '(:amp)))
             (nshell.application:*job-monitor*
               (nshell.domain.job-control:make-job-monitor)))
        (unwind-protect
             (progn
               (with-output-to-string (*standard-output*)
                 (nshell.presentation::execute-ast ast))
               (loop repeat 50
                     until (and (probe-file output)
                                (string= "bg-pipe"
                                         (uiop:read-file-string output)))
                     do (sleep 0.02))
               (let* ((entries (nshell.domain.job-control:monitor-entries
                                nshell.application:*job-monitor*))
                      (job (cdar entries)))
                 (is (= 1 (length entries)))
                 (is (= 2 (length (nshell.domain.execution:job-pids job))))
                 (is (nshell.domain.execution:job-background-p job)))
               (is (probe-file output))
               (is (string= "bg-pipe" (uiop:read-file-string output))))
          (ignore-errors
            (when (probe-file output)
              (delete-file output)))))))

(test reap-background-jobs-removes-only-completed-processes
  "Reaping should update completed jobs and leave live ones alone."
  (with-repl-test-state
      (let* ((monitor (nshell.domain.job-control:make-job-monitor))
             (completed-job (make-test-job 0 "sleep"))
             (alive-job (make-test-job 1 "sleep"))
             (completed-proc :completed)
             (alive-proc :alive)
             (completed-job-id (nshell.domain.job-control:monitor-add-job
                                monitor completed-job))
             (alive-job-id (nshell.domain.job-control:monitor-add-job
                            monitor alive-job)))
        (let ((nshell.application:*job-monitor* monitor)
              (nshell.presentation::*proc-registry*
                (make-hash-table :test #'eql)))
          (setf (gethash completed-job-id nshell.presentation::*proc-registry*)
                completed-proc
                (gethash alive-job-id nshell.presentation::*proc-registry*)
                alive-proc)
          (with-temporary-function
              ('nshell.presentation::%process-alive-p
               (lambda (proc)
                 (eq proc alive-proc)))
            (with-temporary-function
                ('nshell.presentation::%process-exit-code
                 (lambda (proc)
                   (declare (ignore proc))
                   17))
              (nshell.presentation::reap-background-jobs)))
          (is (null (gethash completed-job-id
                             nshell.presentation::*proc-registry*)))
          (is (eq alive-proc
                  (gethash alive-job-id nshell.presentation::*proc-registry*)))
          (is (eq :completed (nshell.domain.execution:job-state completed-job)))
          (is (= 17 (nshell.domain.execution:job-exit-code completed-job)))
          (is (eq :created (nshell.domain.execution:job-state alive-job)))
          (let ((output (with-output-to-string (*standard-output*)
                          (nshell.application:jobs))))
            (is (search "[0] Done sleep" output))
            (is (search "[1] Created sleep" output)))))))
