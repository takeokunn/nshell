(in-package #:nshell/test)

(in-suite parser-tests)

(test parse-incomplete-quote
  (with-first-parsed-diagnostic (diagnostic result "echo 'hello")
    (assert-parsed-diagnostic result diagnostic
                              :present t
                              :incomplete t
                              :kind :unterminated-quote
                              :span-start 5
                              :span-end 11)))

(test parse-incomplete-continuation-operators
  (dolist (line '("echo hello |"
                  "echo hello &&"
                  "echo hello ||"))
    (with-parsed-command-line (result line)
      (with-last-parsed-diagnostic (diagnostic result line)
        (assert-parsed-diagnostic result diagnostic
                                  :present t
                                  :incomplete t
                                  :kind :trailing-continuation)
        (is (not (nshell.domain.parsing:parse-complete-p result))
            "~s should explain the continuation point" line)))))

(test parse-leading-operator-diagnostic
  (let ((line "| grep foo"))
    (with-first-parsed-diagnostic (diagnostic result line)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :kind :missing-command
                                :span-start 0
                                :span-end 1))))

(test parse-leading-redirect-diagnostic
  (let ((line "> out.txt"))
    (with-first-parsed-diagnostic (diagnostic result line)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :kind :missing-command
                                :span-start 0
                                :span-end 1))))

(test parse-trailing-redirect-diagnostic
  (let ((line "echo >"))
    (with-first-parsed-diagnostic (diagnostic result line)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :kind :missing-redirection-target
                                :span-start 5
                                :span-end 6))))

(test parse-redirect-before-separator-diagnostic
  (let ((line "echo > | cat"))
    (with-parsed-diagnostic-of-kind (redirect-diagnostic result line :missing-redirection-target)
      (assert-parsed-diagnostic result redirect-diagnostic
                                :present t
                                :kind :missing-redirection-target
                                :span-start 5
                                :span-end 6))))

(test parse-bare-parenthesis-diagnostic
  (let ((line "("))
    (with-first-parsed-diagnostic (diagnostic result line)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :kind :unexpected-token
                                :span-start 0
                                :span-end 1))))

(test parse-trailing-backslash-is-incomplete
  (let ((line "echo \\"))
    (with-first-parsed-diagnostic (diagnostic result line)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :incomplete t
                                :kind :trailing-escape
                                :span-start 5
                                :span-end 6))))

(test parse-unbalanced-process-substitution-is-incomplete
  (let ((line "cat <(echo ok"))
    (with-first-parsed-diagnostic (diagnostic result line)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :incomplete t
                                :kind :unterminated-process-substitution
                                :span-start 4
                                :span-end 13))))

(test format-parse-diagnostic-lines
  (with-parsed-command-line (result "echo |")
    (is (equal '("nshell: syntax error: Expected command after '|' at column 6")
               (nshell.presentation::format-parse-diagnostic-lines result)))))
