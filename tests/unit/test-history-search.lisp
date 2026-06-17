(in-package #:nshell/test)

(in-suite history-domain-tests)

(test history-prefix-search
  "Prefix search finds matching entries."
  (let* ((history (history-with-lines "git status" "git push" "ls -la"))
         (results (nshell.domain.history:history-search history "git" :mode :prefix)))
    (is (= 2 (length results)))))

(test history-contains-search
  "Contains search finds substring matches."
  (let* ((history (history-with-lines "docker-compose up" "docker ps" "ls"))
         (results (nshell.domain.history:history-search history "docker" :mode :contains)))
    (is (= 2 (length results)))))

(test history-line-prefix-search-matches-continuation-lines
  "Line-prefix search finds matches after a newline in a multi-line entry."
    (let* ((history (history-with-lines "echo setup
git status"
                                      "printf 'not a prefix git'"
                                      "git push"))
         (results (nshell.domain.history:history-search history "git" :mode :line-prefix)))
    (is (equal '("git push" "echo setup
git status")
               (history-result-texts results)))))

(test history-line-prefix-search-respects-smartcase
  "Line-prefix smartcase keeps uppercase queries case-sensitive."
    (let* ((history (history-with-lines "echo setup
git status"
                                      "Git status"))
         (results (nshell.domain.history:history-search history "Git"
                                                        :mode :line-prefix
                                                        :smartcase t)))
    (is (equal '("Git status")
               (history-result-texts results)))))

(test history-smartcase
  "Smartcase makes uppercase queries case-sensitive."
  (let* ((history (history-with-lines "Git Status" "git push"))
         (results (nshell.domain.history:history-search history "Git"
                                                        :mode :prefix
                                                        :smartcase t)))
    (is (= 1 (length results)))
    (is (string= "Git Status"
                 (nshell.domain.history:entry-text (first results))))))
