(in-package #:nshell/test)

(def-suite tokenizer-tests
  :description "Tokenizer unit tests"
  :in nshell-tests)

(in-suite tokenizer-tests)

(test simple-command
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "ls -la")
    (declare (ignore cursor incomplete))
    (is (= 2 (length tokens)))
    (is (string= "ls" (nshell.domain.parsing:token-value (first tokens))))))

(test pipeline
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "ls | grep foo")
    (declare (ignore cursor incomplete))
    (is (= 4 (length tokens)))
    (is (eq :pipe (nshell.domain.parsing:token-type (second tokens))))))

(test redirect
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "echo hello > file.txt")
    (declare (ignore cursor incomplete))
    (is (eq :redirect (nshell.domain.parsing:token-type (third tokens))))))

(test double-quoted-string
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "echo \"hello world\"")
    (declare (ignore cursor incomplete))
    (is (string= "hello world" (nshell.domain.parsing:token-value (second tokens))))))

(test incomplete-quote
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "echo 'hello")
    (declare (ignore tokens cursor))
    (is (not (null incomplete)))))

(test append-redirect
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "echo >> log")
    (declare (ignore cursor incomplete))
    (is (string= ">>" (nshell.domain.parsing:token-value (second tokens))))))

(test empty-input
  (multiple-value-bind (tokens cursor incomplete)
      (nshell.domain.parsing:tokenize "")
    (declare (ignore cursor incomplete))
    (is (null tokens))))

(test pbt-tokenizer-spans-are-monotonic-and-in-bounds
  "Token spans are monotonic and remain within the generated input bounds."
  (for-all-property (:trials 50) ((input (gen-shell-pipeline)))
    (multiple-value-bind (tokens cursor incomplete)
        (nshell.domain.parsing:tokenize input)
      (declare (ignore cursor incomplete))
      (is (loop with previous-end = 0
                for token in tokens
                for start = (nshell.domain.parsing:token-start token)
                for end = (nshell.domain.parsing:token-end token)
                always (and (<= 0 start end (length input))
                            (<= previous-end start))
                do (setf previous-end end))
          "Tokenizer produced non-monotonic or out-of-bounds spans for ~s"
          input))))
