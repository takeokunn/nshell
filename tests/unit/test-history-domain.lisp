(in-package #:nshell/test)

(def-suite history-domain-tests
  :description "History domain tests"
  :in nshell-tests)

(in-suite history-domain-tests)

(test history-entry-creation
  "History entries can be created"
  (let ((entry (nshell.domain.history:make-history-entry "ls -la")))
    (is (string= "ls -la" (nshell.domain.history:entry-text entry)))
    (is (integerp (nshell.domain.history:entry-timestamp entry)))
    (is (null (nshell.domain.history:entry-exit-code entry)))))

(test history-entry-with-exit-code
  "Entry can store exit code"
  (let ((entry (nshell.domain.history:make-history-entry "false" 0 1)))
    (is (= 1 (nshell.domain.history:entry-exit-code entry)))))

(test history-add-and-retrieve
  "Commands added to history can be retrieved"
  (let ((h (nshell.domain.history:make-command-history :max-entries 100)))
    (nshell.domain.history:history-add h "ls -la")
    (nshell.domain.history:history-add h "git status")
    (is (= 2 (nshell.domain.history:history-size h)))
    (is (not (nshell.domain.history:history-empty-p h)))))

(test history-empty
  "New history is empty"
  (let ((h (nshell.domain.history:make-command-history)))
    (is (nshell.domain.history:history-empty-p h))
    (is (= 0 (nshell.domain.history:history-size h)))))

(test history-dedup
  "Adding same command twice keeps only most recent"
  (let ((h (nshell.domain.history:make-command-history :max-entries 100)))
    (nshell.domain.history:history-add h "ls")
    (nshell.domain.history:history-add h "ls")
    (is (= 1 (nshell.domain.history:history-size h)))))

(test history-prefix-search
  "Prefix search finds matching entries"
  (let ((h (nshell.domain.history:make-command-history :max-entries 100)))
    (nshell.domain.history:history-add h "git status")
    (nshell.domain.history:history-add h "git push")
    (nshell.domain.history:history-add h "ls -la")
    (let ((results (nshell.domain.history:history-search h "git" :mode :prefix)))
      (is (= 2 (length results))))))

(test history-contains-search
  "Contains search finds substring matches"
  (let ((h (nshell.domain.history:make-command-history :max-entries 100)))
    (nshell.domain.history:history-add h "docker-compose up")
    (nshell.domain.history:history-add h "docker ps")
    (nshell.domain.history:history-add h "ls")
    (let ((results (nshell.domain.history:history-search h "docker" :mode :contains)))
      (is (= 2 (length results))))))

(test history-smartcase
  "Smartcase: uppercase query = case-sensitive"
  (let ((h (nshell.domain.history:make-command-history :max-entries 100)))
    (nshell.domain.history:history-add h "Git Status")
    (nshell.domain.history:history-add h "git push")
    (let ((results (nshell.domain.history:history-search h "Git" :mode :prefix :smartcase t)))
      (is (= 1 (length results)))
      (is (string= "Git Status"
                   (nshell.domain.history:entry-text (first results)))))))

(test history-max-entries
  "History respects max-entries limit"
  (let ((h (nshell.domain.history:make-command-history :max-entries 3)))
    (nshell.domain.history:history-add h "cmd1")
    (nshell.domain.history:history-add h "cmd2")
    (nshell.domain.history:history-add h "cmd3")
    (nshell.domain.history:history-add h "cmd4")
    (is (= 3 (nshell.domain.history:history-size h)))
    (is (string= "cmd4" (nshell.domain.history:entry-text
                         (first (nshell.domain.history:history-all h)))))))
