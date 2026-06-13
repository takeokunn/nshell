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

;; ── PBT: Tokenizer round-trip property ──
;; Uses a simple seeded LCG for deterministic random generation
(defvar *pbt-seed* 42)

(defun pbt-rand (seed)
  (let ((next (mod (+ (* 1103515245 seed) 12345) (expt 2 31))))
    (values next (mod next 256))))

(defun pbt-random-char (seed)
  (multiple-value-bind (s r) (pbt-rand seed)
    (let ((c (code-char (+ 97 (mod r 26)))))
      (values s c))))

(defun pbt-random-word (seed len)
  (let ((chars '()))
    (dotimes (i len)
      (multiple-value-bind (s c) (pbt-random-char seed)
        (push c chars)
        (setf seed s)))
    (values seed (coerce (nreverse chars) 'string))))

(defun pbt-generate-command (seed)
  "Generate a random command string."
  (multiple-value-bind (s1 word1) (pbt-random-word seed (1+ (mod seed 8)))
    (multiple-value-bind (s2 word2) (pbt-random-word s1 (1+ (mod s1 5)))
      (values s2 (format nil "~a ~a" word1 word2)))))

(test pbt-tokenizer-roundtrip
  "Generated commands tokenize and parse without error (property test)"
  (let ((seed *pbt-seed*))
    (dotimes (i 20)
      (multiple-value-bind (s cmd) (pbt-generate-command seed)
        (setf seed s)
        (multiple-value-bind (tokens cursor incomplete)
            (nshell.domain.parsing:tokenize cmd)
          (declare (ignore cursor))
          (is (not incomplete) "Generated command ~s should not be incomplete" cmd)
          (is (consp tokens) "Generated command ~s should produce tokens" cmd))))))

(test pbt-parse-roundtrip
  "Generated commands parse without error (property test)"
  (let ((seed *pbt-seed*))
    (dotimes (i 20)
      (multiple-value-bind (s cmd) (pbt-generate-command seed)
        (setf seed s)
        (let ((result (nshell.domain.parsing:parse-command-line cmd)))
          (is (nshell.domain.parsing:parse-complete-p result)
              "Generated command ~s should parse completely" cmd)
          (is (not (null (nshell.domain.parsing:parse-result-ast result)))
              "Generated command ~s should produce AST" cmd))))))
