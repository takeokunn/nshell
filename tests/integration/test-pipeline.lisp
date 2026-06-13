(in-package #:nshell/test)

(def-suite integration-tests
  :description "Integration tests for nshell"
  :in nshell-tests)

(in-suite integration-tests)

;; ── Parser Integration Tests ──
(test parse-and-execute-roundtrip
  "Commands can be parsed and produce valid AST"
  (let ((result (nshell.domain.parsing:parse-command-line "echo hello")))
    (is (nshell.domain.parsing:parse-complete-p result))
    (let ((ast (nshell.domain.parsing:parse-result-ast result)))
      (is (nshell.domain.parsing:command-node-p ast)))))

(test pipeline-parsing
  "Pipeline syntax produces pipeline-node AST"
  (let ((result (nshell.domain.parsing:parse-command-line "ls | grep foo")))
    (is (nshell.domain.parsing:parse-complete-p result))
    (let ((ast (nshell.domain.parsing:parse-result-ast result)))
      (is (nshell.domain.parsing:pipeline-node-p ast))
      (is (= 2 (length (nshell.domain.parsing:pipeline-node-commands ast)))))))

;; ── History Integration Tests ──
(test history-search-and-persistence
  "History can store and search commands"
  (let ((h (nshell.domain.history:make-command-history :max-entries 100)))
    (nshell.domain.history:history-add h "git status")
    (nshell.domain.history:history-add h "git push origin main")
    (nshell.domain.history:history-add h "ls -la")
    (let ((results (nshell.domain.history:history-search h "git" :mode :prefix)))
      (is (= 2 (length results))))
    (let ((results (nshell.domain.history:history-search h "ls" :mode :prefix)))
      (is (= 1 (length results))))))

;; ── Unification Integration Tests ──
(test unification-backtracking-integration
  "Unification engine works with backtracking for pattern matching"
  (let* ((x (nshell.domain.parsing:make-var "X"))
         (y (nshell.domain.parsing:make-var "Y"))
         (goal1 (lambda (b) (nshell.domain.parsing:unify x 'command b)))
         (goal2 (lambda (b) (nshell.domain.parsing:unify y 'ls b)))
         (result (nshell.domain.parsing:backtrack (list goal1 goal2))))
    (is (not (null result)))
    (is (eq 'command (nshell.domain.parsing:walk x result)))
    (is (eq 'ls (nshell.domain.parsing:walk y result)))))

;; ── Completion Integration Test ──
(test completion-knowledge-base-integration
  "Knowledge base can store and query commands"
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "git" :subcommands '("status") :flags '("-m"))
    (let ((entry (nshell.domain.completion:kb-query kb "git")))
      (is entry))))

;; ── CPS Trampoline Integration Test ──
(test cps-trampoline-execution
  "CPS trampoline sequentially executes continuations"
  (let ((results '()))
    (nshell.presentation:trampoline
     (lambda ()
       (push 1 results)
       (lambda ()
         (push 2 results)
         (lambda ()
           (push 3 results)
           (nshell.presentation:done)))))
    (is (equal '(3 2 1) results))))

;; ── E2E: Tokenizer → Parser → AST roundtrip ──
(test tokenizer-parser-ast-roundtrip
  "Tokenizer, parser, and AST work together end-to-end"
  (let ((inputs '("ls" "echo hello" "git status" "ls -la | grep foo")))
    (dolist (input inputs)
      (let ((result (nshell.domain.parsing:parse-command-line input)))
        (is (nshell.domain.parsing:parse-complete-p result))
        (is (not (null (nshell.domain.parsing:parse-result-ast result))))))))
