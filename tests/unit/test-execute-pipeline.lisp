(in-package #:nshell/test)

(def-suite execute-pipeline-service-tests
  :description "Application execute-pipeline service tests"
  :in nshell-tests)

(in-suite execute-pipeline-service-tests)

(test execute-command-line-adds-complete-commands-to-history
  "A complete command line returns an AST/result pair and records history."
  (let ((history (nshell.domain.history:make-command-history))
        (dispatcher (nshell.application:make-event-dispatcher)))
    (multiple-value-bind (ast result)
        (nshell.application:execute-command-line "echo hello" history dispatcher)
      (is (nshell.domain.parsing:parse-complete-p result))
      (is (nshell.domain.parsing:command-node-p ast))
      (is (= 1 (nshell.domain.history:history-size history)))
      (is (string= "echo hello"
                   (nshell.domain.history:entry-text
                    (first (nshell.domain.history:history-all history))))))))

(test execute-command-line-does-not-record-incomplete-input
  "Incomplete input returns no AST/result values and leaves history unchanged."
  (let ((history (nshell.domain.history:make-command-history)))
    (multiple-value-bind (ast result)
        (nshell.application:execute-command-line "echo 'unterminated" history nil)
      (is (null ast))
      (is (null result))
      (is (= 0 (nshell.domain.history:history-size history))))))

(test execute-pipeline-use-case-runs-command-and-publishes-events
  "The execute-pipeline use case returns the exit status and emits lifecycle events."
  (let ((dispatcher (nshell.application:make-event-dispatcher))
        (events nil)
        (ast (nshell.domain.parsing:make-command-node "true" nil)))
    (dolist (type '(:pipeline-started :process-created :process-exited :pipeline-completed))
      (nshell.application:subscribe dispatcher type
                                    (lambda (event)
                                      (push (nshell.domain.events:event-type event) events))))
    (is (= 0 (nshell.application:execute-pipeline-use-case ast dispatcher)))
    (is (null (nshell.application:drain-events dispatcher)))
    (let ((delivered (nreverse events)))
      (is (member :pipeline-started delivered))
      (is (member :pipeline-completed delivered)))))

(test execute-pipeline-use-case-applies-stage-redirections
  "Pipeline execution through the application API preserves per-stage redirects."
  (let* ((root (merge-pathnames (format nil "nshell-app-pipeline-redir-~d/"
                                         (random 1000000))
                                (uiop:temporary-directory)))
         (output (merge-pathnames "output.txt" root))
         (content "application pipeline redirection")
         (ast (nshell.domain.parsing:make-pipeline-node
               (list
                (nshell.domain.parsing:make-command-node "printf" (list content))
                (nshell.domain.parsing:make-command-node
                 "cat"
                 (list ">" (namestring output)))))))
    (unwind-protect
         (progn
           (ensure-directories-exist root)
           (is (= 0 (nshell.application:execute-pipeline-use-case ast nil)))
           (is (probe-file output))
           (with-open-file (stream output :direction :input)
             (let ((actual (make-string (file-length stream))))
               (read-sequence actual stream)
               (is (string= content actual)))))
      (handler-case
          (when (probe-file root)
            (uiop:delete-directory-tree root :validate t))
        (error ())))))

(test execute-pipeline-use-case-returns-127-for-missing-command
  "A pipeline with an unresolvable command reports a non-zero spawn failure."
  (let ((ast (nshell.domain.parsing:make-command-node
              "definitely-not-a-real-command"
              nil)))
    (is (= 127 (nshell.application:execute-pipeline-use-case ast nil)))))
