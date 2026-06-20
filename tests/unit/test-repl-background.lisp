(in-package #:nshell/test)

(in-suite repl-tests)

(test repl-extract-redirects-preserves-dangling-operator
  "Malformed redirect argument lists should not make redirect extraction loop forever."
  (dolist (case '((("echo" ">")
                   ("echo" ">")
                   ())
                  (("echo" ">" "out.txt")
                   ("echo")
                   ((:> . "out.txt")))
                  (("echo" ">>" "out.txt")
                   ("echo")
                   ((:>> . "out.txt")))
                  (("echo" "<" "in.txt")
                   ("echo")
                   ((:< . "in.txt")))))
    (destructuring-bind (args expected-clean expected-redirects) case
      (multiple-value-bind (clean redirects)
          (nshell.presentation::extract-redirects args)
        (is (equal expected-clean clean))
        (is (equal expected-redirects redirects))))))

(test repl-background-command-applies-redirections
  "Background commands should apply redirects before spawning the process."
  (with-repl-test-state
      (let ((nshell.application:*job-monitor*
              (nshell.domain.job-control:make-job-monitor)))
        (with-temporary-output-file (output :prefix "nshell-bg-redirect-")
          (let ((ast (nshell.domain.parsing::make-sequence-node
                      (list (nshell.domain.parsing:make-command-node
                             "printf"
                             (list (cons "%s" t) "bg" ">" output)))
                      '(:amp))))
            (multiple-value-bind (output-text code)
                (call-repl-execute-ast ast)
              (declare (ignore output-text code)))
            (is (wait-for-file-content output "bg"))
            (is (probe-file output))
            (is (string= "bg" (uiop:read-file-string output))))))))

(test repl-background-pipeline-registers-processes-and-applies-redirections
  "Background pipelines should spawn every stage and keep their redirects."
  (with-repl-test-state
      (let ((nshell.application:*job-monitor*
              (nshell.domain.job-control:make-job-monitor)))
        (with-temporary-output-file (output :prefix "nshell-bg-pipeline-")
          (let* ((pipeline (nshell.domain.parsing:make-pipeline-node
                            (list (nshell.domain.parsing:make-command-node
                                   "printf"
                                   (list (cons "%s" t) "bg-pipe"))
                                  (nshell.domain.parsing:make-command-node
                                   "cat"
                                   (list ">" output)))))
                 (ast (nshell.domain.parsing::make-sequence-node
                       (list pipeline)
                       '(:amp))))
            (multiple-value-bind (output-text code)
                (call-repl-execute-ast ast)
              (declare (ignore output-text code)))
            (is (wait-for-file-content output "bg-pipe"))
            (let* ((entries (nshell.domain.job-control:monitor-entries
                             nshell.application:*job-monitor*))
                   (job (cdar entries)))
              (is (= 1 (length entries)))
              (is (= 2 (length (nshell.domain.execution:job-pids job))))
              (is (nshell.domain.execution:job-background-p job)))
            (is (probe-file output))
            (is (string= "bg-pipe" (uiop:read-file-string output))))))))

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
            (let ((nshell.presentation::*background-proc-alive-p*
                    (lambda (proc)
                      (eq proc alive-proc)))
                  (nshell.presentation::*background-proc-exit-code*
                    (lambda (proc)
                      (declare (ignore proc))
                      17)))
              (nshell.presentation::reap-background-jobs))
            (is (null (gethash completed-job-id
                               nshell.presentation::*proc-registry*)))
            (is (eq alive-proc
                  (gethash alive-job-id nshell.presentation::*proc-registry*)))
          (is (eq :completed (nshell.domain.execution:job-state completed-job)))
          (is (= 17 (nshell.domain.execution:job-exit-code completed-job)))
          (is (eq :created (nshell.domain.execution:job-state alive-job)))
          (let ((output (capture-standard-output
                          (nshell.application:jobs))))
              (is (search "[0] Done sleep" output))
              (is (search "[1] Created sleep" output)))))))

(test reap-background-jobs-handles-process-lists
  "Reaping should treat process lists as a single background job entry."
  (with-repl-test-state
      (let* ((monitor (nshell.domain.job-control:make-job-monitor))
             (completed-job (make-test-job 0 "sleep"))
             (alive-job (make-test-job 1 "sleep"))
             (completed-proc-1 :completed-1)
             (completed-proc-2 :completed-2)
             (alive-proc :alive)
             (completed-job-id (nshell.domain.job-control:monitor-add-job
                                monitor completed-job))
             (alive-job-id (nshell.domain.job-control:monitor-add-job
                            monitor alive-job)))
        (let ((nshell.application:*job-monitor* monitor)
              (nshell.presentation::*proc-registry*
                (make-hash-table :test #'eql)))
          (setf (gethash completed-job-id nshell.presentation::*proc-registry*)
                  (list completed-proc-1 completed-proc-2)
                (gethash alive-job-id nshell.presentation::*proc-registry*)
                  (list completed-proc-1 alive-proc))
          (let ((nshell.presentation::*background-proc-alive-p*
                  (lambda (proc)
                    (eq proc alive-proc)))
                (nshell.presentation::*background-proc-exit-code*
                  (lambda (proc)
                    (case proc
                      (:completed-1 11)
                      (:completed-2 23)
                      (t 0)))))
            (nshell.presentation::reap-background-jobs))
          (is (null (gethash completed-job-id
                             nshell.presentation::*proc-registry*)))
          (is (equal (list completed-proc-1 alive-proc)
                     (gethash alive-job-id nshell.presentation::*proc-registry*)))
          (is (eq :completed (nshell.domain.execution:job-state completed-job)))
          (is (= 23 (nshell.domain.execution:job-exit-code completed-job)))
          (is (eq :created (nshell.domain.execution:job-state alive-job)))))))
