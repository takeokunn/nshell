(in-package #:nshell/test)

(def-suite completion-rules-tests
  :description "Rule-based completion engine tests"
  :in nshell-tests)

(in-suite completion-rules-tests)

(defun make-empty-rule-kb ()
  (nshell.domain.completion::make-rule-knowledge-base))

(defun solution-binding (variable solution)
  (cdr (assoc variable solution)))

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

(test occurs-check-prevents-infinite-loops
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'recursive :args '(?x (wrap ?x))))
    (is (null (nshell.domain.completion:prove-all kb '(recursive ?x ?x))))))

(test cd-completes-directories-integration
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "cd ")))
    (is (= 1 (length candidates)))
    (is (eq :directory (nshell.domain.completion:candidate-kind (first candidates))))))

(test command-flags-are-completed
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "ls --")))
    (is (member "--help" (mapcar #'nshell.domain.completion:candidate-text candidates)
                :test #'string=))))

(test hash-table-completion-backward-compatibility
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "custom" :flags '("--custom"))
    (let ((commands (nshell.domain.completion:complete kb "cu"))
          (arguments (nshell.domain.completion:complete-argument kb "custom" "--c")))
      (is (= 1 (length commands)))
      (is (string= "custom" (nshell.domain.completion:candidate-text (first commands))))
      (is (equal '("--custom") arguments)))))
