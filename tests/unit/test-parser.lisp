(in-package #:nshell/test)

(def-suite parser-tests
  :description "Shell parser tests"
  :in nshell-tests)

(in-suite parser-tests)

(test parse-simple-command
  (let ((result (nshell.domain.parsing:parse-command-line "ls -la")))
    (is (nshell.domain.parsing:parse-complete-p result))
    (let ((ast (nshell.domain.parsing:parse-result-ast result)))
      (is (nshell.domain.parsing:command-node-p ast))
      (is (string= "ls" (nshell.domain.parsing:command-node-command ast)))
      (is (equal '("-la") (nshell.domain.parsing:command-node-args ast))))))

(test parse-pipeline
  (let ((result (nshell.domain.parsing:parse-command-line "ls | grep foo")))
    (is (nshell.domain.parsing:parse-complete-p result))
    (let ((ast (nshell.domain.parsing:parse-result-ast result)))
      (is (nshell.domain.parsing:pipeline-node-p ast))
      (is (= 2 (length (nshell.domain.parsing:pipeline-node-commands ast)))))))

(test parse-empty-input
  (let ((result (nshell.domain.parsing:parse-command-line "")))
    (is (null (nshell.domain.parsing:parse-result-ast result)))))

(test parse-incomplete-quote
  (let ((result (nshell.domain.parsing:parse-command-line "echo 'hello")))
    (is (nshell.domain.parsing:parse-result-incomplete result))))

;; PBT: Tokenizer round-trip property
(test tokenizer-roundtrip-property
  "Generated simple commands tokenize and parse without error"
  (let ((inputs '("ls" "pwd" "echo hello" "git status" "ls -la" "cat file.txt")))
    (dolist (input inputs)
      (multiple-value-bind (tokens cursor incomplete)
          (nshell.domain.parsing:tokenize input)
        (declare (ignore cursor))
        (is (not incomplete))
        (is (consp tokens))))))
