(in-package #:nshell/test)

(in-suite parser-tests)

(test parse-simple-command
  (with-complete-ast (ast "ls -la")
    (is (nshell.domain.parsing:command-node-p ast))
    (is (string= "ls" (nshell.domain.parsing:command-node-command ast)))
    (is (equal '("-la") (nshell.domain.parsing:command-node-args ast)))))

(test parse-records-quote-style-per-argument
  "Single and double quotes are distinguished so expansion can treat them differently."
  (with-complete-ast (ast "echo plain \"$FOO\" '*'")
    (let ((args (nshell.domain.parsing:command-node-args ast)))
      (is (= 3 (length args)))
      ;; Unquoted word: no quote style, stored as a bare string.
      (is (null (nshell.domain.parsing:arg-quote-style (first args))))
      ;; Double-quoted: expandable (not arg-quoted-p) but flagged :double.
      (is (eq :double (nshell.domain.parsing:arg-quote-style (second args))))
      (is (not (nshell.domain.parsing:arg-quoted-p (second args))))
      ;; Single-quoted: fully literal.
      (is (eq :single (nshell.domain.parsing:arg-quote-style (third args))))
      (is (nshell.domain.parsing:arg-quoted-p (third args))))))

(test parse-pipeline
  (with-complete-ast (ast "ls | grep foo")
    (is (nshell.domain.parsing:pipeline-node-p ast))
    (is (= 2 (length (nshell.domain.parsing:pipeline-node-commands ast))))))

(test parse-mixed-sequence-and-pipeline
  (with-complete-ast (ast "echo one | cat; echo two")
    (is (nshell.domain.parsing:sequence-node-p ast))
    (is (= 2 (length (nshell.domain.parsing:sequence-node-commands ast))))
    (is (nshell.domain.parsing:pipeline-node-p
         (first (nshell.domain.parsing:sequence-node-commands ast))))
    (is (nshell.domain.parsing:command-node-p
         (second (nshell.domain.parsing:sequence-node-commands ast))))
    (is (equal '(:semi)
               (nshell.domain.parsing:sequence-node-separators ast)))))

(test parse-empty-input
  (with-parsed-command-line (result "")
    (is (null (nshell.domain.parsing:parse-result-ast result)))))

(test parse-complete-redirect
  (with-complete-command-line (result ast "echo hello > out.txt")
    (is (null (nshell.domain.parsing:parse-errors result)))
    (is (nshell.domain.parsing:command-node-p ast))
    (is (equal '("hello" (">" . nil) "out.txt")
               (nshell.domain.parsing:command-node-args ast)))))

(test parse-escaped-space-word
  (with-complete-command-line (result ast "echo hello\\ world")
    (is (null (nshell.domain.parsing:parse-errors result)))
    (is (nshell.domain.parsing:command-node-p ast))
    (is (equal '("hello world")
               (nshell.domain.parsing:command-node-args ast)))))
