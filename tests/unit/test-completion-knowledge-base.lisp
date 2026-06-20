(in-package #:nshell/test)

(in-suite completion-rules-tests)

(test knowledge-base-completion-uses-explicit-command-facts
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "custom" :flags '("--custom"))
    (let ((commands (nshell.domain.completion:complete kb "cu"))
          (arguments (completion-texts (nshell.domain.completion:complete kb "custom --c"))))
      (is (= 1 (length commands)))
      (is (string= "custom" (nshell.domain.completion:candidate-text (first commands))))
      (is (equal '("--custom") arguments)))))

(test knowledge-base-command-completion-carries-description
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "deploy" :description "release service")
    (let ((candidate (completion-candidate-by-text
                      "deploy"
                      (nshell.domain.completion:complete kb "dep"))))
      (is (not (null candidate)))
      (is (string= "release service"
                   (nshell.domain.completion:candidate-description candidate))))))

(test path-command-completion-merges-with-kb-and-path-candidates
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "cargo")
    (with-path-command-adapters
        ((lambda (directory)
           (declare (ignore directory))
           (list #p"/mock/cat" #p"/mock/cargo" #p"/mock/readme"))
         (lambda (entry)
           (not (string= "readme" (file-namestring entry)))))
      (let ((texts (completion-texts
                    (nshell.domain.completion:complete kb "c" :path "/mock:/other"))))
        (is (equal '("cd" "complete" "contains" "count" "cargo" "cat") texts))))))

(test command-completion-ranks-exact-match-first
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "git")
    (nshell.domain.completion:kb-add-command kb "gitk")
    (nshell.domain.completion:kb-add-command kb "gist")
    (let ((texts (completion-texts
                  (nshell.domain.completion:complete kb "git"))))
      (is (equal '("git" "gitk") texts)))))

(test command-completion-ranks-case-sensitive-prefix-before-case-folded-match
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "ZZCase-tool")
    (nshell.domain.completion:kb-add-command kb "zzcase-tool")
    (let ((texts (completion-texts
                  (nshell.domain.completion:complete kb "zzcase"))))
      (is (equal '("zzcase-tool" "ZZCase-tool") texts)))))

(test command-completion-keeps-best-duplicate-metadata
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "tool" :description "managed command")
    (with-path-command-adapters
        ((lambda (directory)
           (declare (ignore directory))
           (list #p"/mock/tool"))
         (constantly t))
      (let ((candidates (nshell.domain.completion:complete kb "to" :path "/mock")))
        (is (= 1 (length candidates)))
        (is (string= "tool"
                     (nshell.domain.completion:candidate-text (first candidates))))
        (is (string= "managed command"
                     (nshell.domain.completion:candidate-description
                      (first candidates))))))))

(test path-command-completion-ignores-argument-position
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (with-path-command-adapters
        ((lambda (directory)
           (declare (ignore directory))
           (list #p"/mock/git"))
         (constantly t))
      (is (null (nshell.domain.completion:complete kb "echo g" :path "/mock"))))))

(test path-command-completion-skips-directory-prefixed-commands
  (let ((kb (nshell.domain.completion:make-knowledge-base)))
    (with-path-command-adapters
        ((lambda (directory)
           (declare (ignore directory))
           (list #p"/mock/git"))
         (constantly t))
      (is (null (nshell.domain.completion:complete kb "./g" :path "/mock"))))))
