(in-package #:nshell/test)

(in-suite history-domain-tests)

(test history-entry-creation
  "History entries can be created."
  (let ((entry (nshell.domain.history:make-history-entry "ls -la")))
    (is (string= "ls -la" (nshell.domain.history:entry-text entry)))
    (is (integerp (nshell.domain.history:entry-timestamp entry)))
    (is (null (nshell.domain.history:entry-exit-code entry)))))

(test history-entry-with-exit-code
  "Entry can store exit code."
  (let ((entry (nshell.domain.history:make-history-entry "false" 0 1)))
    (is (= 1 (nshell.domain.history:entry-exit-code entry)))))
