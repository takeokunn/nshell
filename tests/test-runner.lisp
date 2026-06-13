;;; nshell test runner
;;; Aggregates and runs all test suites

(in-package #:nshell/test)

(def-suite nshell-tests
  :description "nshell test suite - all tests")

(in-suite nshell-tests)

(test smoke-test
  "Basic sanity check that the test framework and project are loaded correctly."
  (is (= 1 1))
  (is (string= "nshell" "nshell")))

(defun run-tests ()
  "Run all nshell tests."
  (run! 'nshell-tests))
