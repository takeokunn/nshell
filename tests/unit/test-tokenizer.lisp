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
    (is incomplete)))

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
