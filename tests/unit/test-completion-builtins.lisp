(in-package #:nshell/test)

(in-suite completion-rules-tests)

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
    (is (member "--help" (completion-texts candidates)
                :test #'string=))))

(test rule-completion-candidates-carry-descriptions
  (let* ((candidates (nshell.domain.completion:rule-complete
                      nshell.domain.completion::*built-in-rule-knowledge-base*
                      "git st"))
         (status (completion-candidate-by-text "status" candidates)))
    (is (not (null status)))
    (is (eq :option (nshell.domain.completion:candidate-kind status)))
    (is (string= "show working tree status"
                 (nshell.domain.completion:candidate-description status)))))

(test rule-completion-dedupes-multiple-proof-paths
  (let ((kb (make-empty-rule-kb)))
    (nshell.domain.completion:assert-fact!
     kb
     (nshell.domain.completion:make-fact :predicate 'nshell.domain.completion::completes
                                         :args '("git" "status")))
    (nshell.domain.completion:assert-rule!
     kb
     (nshell.domain.completion:make-rule :head '(nshell.domain.completion::completes
                                                 "git"
                                                 "status")
                                         :body '()))
    (let ((candidates (nshell.domain.completion:rule-complete kb "git st")))
      (is (equal '("status") (completion-texts candidates))))))

(test rule-completion-skips-leading-assignment-words
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "FOO=bar git st")))
    (is (member "status" (completion-texts candidates)
                :test #'string=))))

(test rule-completion-uses-current-pipeline-command
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "echo ready | git st")))
    (is (member "status" (completion-texts candidates)
                :test #'string=))))

(test rule-completion-treats-redirection-targets-as-files
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "git > st")))
    (is (= 1 (length candidates)))
    (is (string= "st" (nshell.domain.completion:candidate-text (first candidates))))
    (is (eq :file (nshell.domain.completion:candidate-kind (first candidates))))
    (is (not (member "status" (completion-texts candidates)
                     :test #'string=)))))

(test rule-completion-treats-empty-redirection-targets-as-files
  (dolist (line '("git >" "git > " "git >> " "git < "))
    (let ((candidates (nshell.domain.completion:rule-complete
                       nshell.domain.completion::*built-in-rule-knowledge-base*
                       line)))
      (is (= 1 (length candidates)))
      (is (eq :file (nshell.domain.completion:candidate-kind (first candidates)))
          line))))

(test complete-redirection-targets-from-filesystem
  (with-file-completion-adapters
      ((lambda (dir)
         (declare (ignore dir))
         (list #p"stderr.log" #p"stdout.txt" #p"notes.txt"))
       (lambda (dir)
         (declare (ignore dir))
         (list #p"staging/")))
    (let* ((candidates (nshell.domain.completion:complete
                        nshell.domain.completion::*built-in-rule-knowledge-base*
                        "echo > st"))
           (texts (completion-texts candidates))
           (kinds (mapcar #'nshell.domain.completion:candidate-kind candidates)))
      (is (equal '("staging/" "stderr.log" "stdout.txt") texts))
      (is (equal '(:directory :file :file) kinds)))))

(test complete-cd-targets-from-filesystem-directories-only
  (with-file-completion-adapters
      ((lambda (dir)
         (declare (ignore dir))
         (list #p"src.log"))
       (lambda (dir)
         (declare (ignore dir))
         (list #p"src/" #p"sandbox/")))
    (let* ((candidates (nshell.domain.completion:complete
                        nshell.domain.completion::*built-in-rule-knowledge-base*
                        "cd s"))
           (texts (completion-texts candidates))
           (kinds (mapcar #'nshell.domain.completion:candidate-kind candidates)))
      (is (equal '("sandbox/" "src/") texts))
      (is (every (lambda (kind) (eq kind :directory)) kinds)))))

(test complete-source-targets-from-filesystem
  (with-file-completion-adapters
      ((lambda (dir)
         (declare (ignore dir))
         (list #p"init.lisp" #p"install.sh" #p"readme.md"))
       (lambda (dir)
         (declare (ignore dir))
         (list #p"included/")))
    (dolist (line '("source in" ". in"))
      (let ((texts (completion-texts
                    (nshell.domain.completion:complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     line))))
        (is (equal '("included/" "init.lisp" "install.sh") texts) line)))))

(test complete-source-targets-from-filesystem-after-trailing-space
  (with-file-completion-adapters
      ((lambda (dir)
         (declare (ignore dir))
         (list #p"init.lisp" #p"install.sh" #p"readme.md"))
       (lambda (dir)
         (declare (ignore dir))
         (list #p"included/")))
    (dolist (line '("source " ". "))
      (let ((texts (completion-texts
                    (nshell.domain.completion:complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     line))))
        (is (equal '("included/" "init.lisp" "install.sh" "readme.md") texts)
            line)))))

(test rule-completion-keeps-quoted-arguments-out-of-prefix
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "git commit -m \"hello world\" --")))
    (is (member "--help" (completion-texts candidates)
                :test #'string=))))

(test rule-completion-keeps-escaped-arguments-out-of-prefix
  (let ((candidates (nshell.domain.completion:rule-complete
                     nshell.domain.completion::*built-in-rule-knowledge-base*
                     "git add my\\ file st")))
    (is (member "status" (completion-texts candidates)
                :test #'string=))))
