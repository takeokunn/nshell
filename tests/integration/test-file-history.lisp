(in-package #:nshell/test)

(def-suite file-history-tests
  :description "File-based history integration tests"
  :in nshell-tests)

(in-suite file-history-tests)

(test file-history-append
  "Appending to file history works"
  (let ((path (nshell.infrastructure.persistence:history-file-path)))
    ;; Clean up any previous test data
    (when (probe-file path) (delete-file path))
    (nshell.infrastructure.persistence:append-history-entry "test command")
    (let ((loaded (nshell.infrastructure.persistence:load-history-file)))
      (is (consp loaded))
      (is (string= "test command" (first loaded))))
    ;; Cleanup
    (delete-file path)))
