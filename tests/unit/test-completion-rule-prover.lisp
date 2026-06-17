(in-package #:nshell/test)

(in-suite completion-rules-tests)

(defmethod nshell.domain.completion:predicate-true-p
    ((predicate (eql 'test-builtin-true)) args bindings)
  (declare (ignore predicate args bindings))
  t)

(defmethod nshell.domain.completion:predicate-true-p
    ((predicate (eql 'test-builtin-string=)) args bindings)
  (declare (ignore predicate bindings))
  (and (= 2 (length args))
       (string= (first args) (second args))))

(test fact-only-resolution
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'completes :args '("ls" "--help")))
    (is (= 1 (length (nshell.domain.completion:prove-all kb '(completes "ls" "--help")))))))

(test rule-with-one-body-goal-resolves
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'command-is :args '("cd" "cd")))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(suggests-dir ?input)
                                          :body '((command-is ?input "cd"))))
    (is (= 1 (length (nshell.domain.completion:prove-all kb '(suggests-dir "cd")))))))

(test rule-with-conjunction-resolves
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'command-is :args '("git" "git")))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'has-flag :args '("git" "--help")))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(documented-command ?cmd)
                                          :body '((command-is ?cmd "git")
                                                  (has-flag ?cmd "--help"))))
    (is (= 1 (length (nshell.domain.completion:prove-all kb '(documented-command "git")))))))

(test rule-disjunction-via-multiple-rules
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'git-subcommand :args '("add")))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'git-subcommand :args '("commit")))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(completes "git" ?sub)
                                          :body '((git-subcommand ?sub))))
    (let ((solutions (nshell.domain.completion:prove-all kb '(completes "git" ?sub))))
      (is (= 2 (length solutions)))
      (is (member "add" (mapcar (lambda (solution) (solution-binding '?sub solution)) solutions)
                  :test #'string=))
      (is (member "commit" (mapcar (lambda (solution) (solution-binding '?sub solution)) solutions)
                  :test #'string=)))))

(test variable-binding-extraction
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'completes :args '("ls" "--help")))
    (let ((solutions (nshell.domain.completion:prove-all kb '(completes ?command "--help"))))
      (is (= 1 (length solutions)))
      (is (string= "ls" (solution-binding '?command (first solutions)))))))

(test no-solution-for-unsatisfiable-goal
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'completes :args '("ls" "--help")))
    (is (null (nshell.domain.completion:prove-all kb '(completes "cat" "--help"))))))

(test builtin-predicate-succeeds-without-bindings
  (let ((solutions
          (nshell.domain.completion:prove-all
           (make-empty-rule-kb)
           '(test-builtin-true))))
    (is (= 1 (length solutions)))
    (is (null (first solutions)))))

(test builtin-predicate-participates-in-rule-body
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'command-is :args '("git" "git")))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(verified-command ?cmd)
                                          :body '((command-is ?cmd "git")
                                                  (test-builtin-string= ?cmd "git"))))
    (let ((solutions
            (nshell.domain.completion:prove-all kb '(verified-command ?cmd))))
      (is (= 1 (length solutions)))
      (is (string= "git" (solution-binding '?cmd (first solutions)))))))

(test builtin-predicate-solutions-are-combined-with-facts
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'test-builtin-true :args '()))
    (is (= 2 (length (nshell.domain.completion:prove-all kb '(test-builtin-true)))))))

(test occurs-check-prevents-infinite-loops
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'recursive :args '(?x (wrap ?x))))
    (is (null (nshell.domain.completion:prove-all kb '(recursive ?x ?x))))))

(test recursive-rule-search-is-depth-bounded
  "Recursive completion rules must not hang the interactive proof engine."
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(loops ?x)
                                          :body '((loops ?x))))
    (is (null (nshell.domain.completion:prove-all kb '(loops "git") :max-depth 4)))))

(test bounded-recursive-rule-keeps-finite-solutions
  "Depth limiting still allows useful transitive completion facts within the bound."
  (let ((kb (make-empty-rule-kb)))
    (dolist (edge '(("git" "status")
                    ("status" "--short")
                    ("--short" "format")))
      (nshell.domain.completion:assert-fact!
       kb
       (nshell.domain.completion:make-fact :predicate 'edge :args edge)))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(reachable ?from ?to)
                                          :body '((edge ?from ?to))))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(reachable ?from ?to)
                                          :body '((edge ?from ?mid)
                                                  (reachable ?mid ?to))))
    (let ((shallow (nshell.domain.completion:prove-all
                    kb '(reachable "git" ?target) :max-depth 1))
          (deep (nshell.domain.completion:prove-all
                 kb '(reachable "git" ?target) :max-depth 4)))
      (is (member "status" (mapcar (lambda (solution)
                                      (solution-binding '?target solution))
                                    shallow)
                  :test #'string=))
      (is (not (member "format" (mapcar (lambda (solution)
                                           (solution-binding '?target solution))
                                         shallow)
                       :test #'string=)))
      (is (member "format" (mapcar (lambda (solution)
                                      (solution-binding '?target solution))
                                    deep)
                  :test #'string=)))))
