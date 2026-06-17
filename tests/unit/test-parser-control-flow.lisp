(in-package #:nshell/test)

(in-suite parser-tests)

(test parse-incomplete-control-flow-blocks
  (do-command-lines (line '("if true"
                            "for item in a b"
                            "while true"
                            "begin"))
    (with-parsed-command-line (result line)
      (with-last-parsed-diagnostic (diagnostic result line)
        (assert-parsed-diagnostic result diagnostic
                                  :present t
                                  :incomplete t
                                  :kind :unclosed-block))))
  (with-parsed-command-line (result "if true; echo ok; end")
    (is (not (nshell.domain.parsing:parse-result-incomplete result)))
    (is (nshell.domain.parsing:parse-complete-p result))))

(test parse-unmatched-control-flow-terminators
  (do-command-lines (line '("else"
                            "end"
                            "if true; else; else; end"))
    (with-parsed-command-line (result line)
      (with-parsed-diagnostic-of-kind (diagnostic result line :unexpected-control-flow)
        (assert-parsed-diagnostic result diagnostic
                                  :present t
                                  :kind :unexpected-control-flow
                                  :within-input t
                                  :line line)
        (is (not (nshell.domain.parsing:parse-complete-p result))
            "~s should not parse completely" line)))))

(test parse-case-outside-switch-is-an-error
  (with-parsed-command-line (result "case vanilla")
    (with-parsed-diagnostic-of-kind (diagnostic result "case vanilla" :unexpected-control-flow)
      (assert-parsed-diagnostic result diagnostic
                                :present t
                                :kind :unexpected-control-flow)
      (is (not (nshell.domain.parsing:parse-complete-p result))))))

(test parse-fish-switch-case-block
  (with-complete-command-line (result ast
                               "switch chocolate; case vanilla; echo plain; case chocolate strawberry; echo sweet; case '*'; echo default; end")
    (let ((clauses (and (nshell.domain.parsing:case-node-p ast)
                        (nshell.domain.parsing:case-node-clauses ast))))
      (is (nshell.domain.parsing:case-node-p ast))
      (is (string= "chocolate" (nshell.domain.parsing:case-node-value ast)))
      (is (equal '("vanilla" "chocolate" "strawberry" "*")
                 (mapcar #'car clauses)))
      (is (string= "echo"
                   (nshell.domain.parsing:command-node-command
                    (first (cdr (second clauses)))))))))

(test parse-else-if-stays-as-else-branch-command
  (with-complete-ast (ast "if true; echo yes; else if false; echo no; end")
    (let ((else-branch (nshell.domain.parsing:if-node-else-branch ast)))
      (is (nshell.domain.parsing:if-node-p ast))
      (is (string= "true"
                   (nshell.domain.parsing:command-node-command
                    (nshell.domain.parsing:if-node-condition ast))))
      (is (= 1 (length else-branch)))
      (is (nshell.domain.parsing:command-node-p (first else-branch)))
      (is (string= "echo"
                   (nshell.domain.parsing:command-node-command
                    (first else-branch))))
      (is (equal '("no")
                 (nshell.domain.parsing:command-node-args
                  (first else-branch)))))))

(test parse-single-command-background-preserves-sequence-node
  "A trailing & on a single command should stay as a sequence node."
  (with-complete-ast (ast "echo hello &")
    (is (nshell.domain.parsing:sequence-node-p ast))
    (is (= 1 (length (nshell.domain.parsing:sequence-node-commands ast))))
    (is (equal '(:amp) (nshell.domain.parsing:sequence-node-separators ast)))))
