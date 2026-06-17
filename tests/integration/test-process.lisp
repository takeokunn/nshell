(in-package #:nshell/test)

(def-suite process-tests
  :description "Process execution integration tests"
  :in nshell-tests)

(in-suite process-tests)

(test run-external-echo
  "External echo command executes and returns exit 0"
  (let ((exit (nshell.infrastructure.acl:run-external "echo" '("hello"))))
    (is (= 0 exit))))

(test run-external-capture-echo
  "External command capture returns stdout and exit code."
  (multiple-value-bind (output exit)
      (nshell.infrastructure.acl:run-external-capture "echo" '("hello"))
    (is (= 0 exit))
    (is (string= (format nil "hello~%") output))))

(test run-external-nonexistent
  "Nonexistent command returns error exit code"
  (let ((exit (nshell.infrastructure.acl:run-external "nonexistent_cmd_xyz" '())))
    (is (not (= 0 exit)))))

(test run-external-capture-nonexistent
  "Nonexistent command capture returns an error exit code and message."
  (multiple-value-bind (output exit)
      (nshell.infrastructure.acl:run-external-capture "nonexistent_cmd_xyz" '())
    (is (not (= 0 exit)))
    (is (search "nonexistent_cmd_xyz" output))))

(test spawn-async-inherits-output-when-unredirected
  "Unredirected background processes should not leave an unread output pipe."
  (let ((proc (nshell.infrastructure.acl:spawn-async
               "true"
               nil)))
    (is (not (null proc)))
    (when proc
      (unwind-protect
           (progn
             (sb-ext:process-wait proc)
             (is (null (sb-ext:process-output proc)))
             (is (= 0 (sb-ext:process-exit-code proc))))
        (when (sb-ext:process-alive-p proc)
          (ignore-errors
            (sb-ext:process-kill proc 15)))))))
