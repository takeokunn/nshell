(in-package #:nshell/test)

(def-suite process-tests
  :description "Process execution integration tests"
  :in nshell-tests)

(in-suite process-tests)

(test run-external-echo
  "External echo command executes and returns exit 0"
  (let ((exit (nshell.infrastructure.acl:run-external "echo" '("hello"))))
    (is (= 0 exit))))

(test run-external-nonexistent
  "Nonexistent command returns error exit code"
  (let ((exit (nshell.infrastructure.acl:run-external "nonexistent_cmd_xyz" '())))
    (is (not (= 0 exit)))))
