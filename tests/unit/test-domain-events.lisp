(in-package #:nshell/test)

(def-suite domain-events-tests
  :description "Domain event unit tests"
  :in nshell-tests)

(in-suite domain-events-tests)

(defmacro assert-event-types (&rest cases)
  `(progn
     ,@(loop for (event-form expected-type) in cases
             collect `(is (eq (nshell.domain.events:domain-event-type ,event-form)
                              ,expected-type)))))

(test event-creation
  "Domain events can be created with correct type"
  (let ((event (nshell.domain.events:make-domain-event :test-event)))
    (is (eq (nshell.domain.events:domain-event-type event) :test-event))
    (is (integerp (nshell.domain.events:domain-event-timestamp event)))))

(test command-events-have-correct-types
  "All command event constructors produce correct types"
  (assert-event-types
   ((nshell.domain.events:make-command-entered-event "ls") :command-entered)
   ((nshell.domain.events:make-command-parsed-event '()) :command-parsed)
   ((nshell.domain.events:make-parse-failed-event "bad" "error") :parse-failed)))

(test job-events-have-correct-types
  "All job event constructors produce correct types"
  (assert-event-types
   ((nshell.domain.events:make-job-created-event 1 "ls" 100) :job-created)
   ((nshell.domain.events:make-job-stopped-event 1 :sigterm) :job-stopped)
   ((nshell.domain.events:make-job-completed-event 1 0) :job-completed)))

(test event-timestamp-is-monotonic
  "Event timestamps are set at creation time"
  (let* ((t1 (get-universal-time))
         (event (nshell.domain.events:make-domain-event :test)))
    (is (<= t1 (nshell.domain.events:domain-event-timestamp event)))))
