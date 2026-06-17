(in-package #:nshell/test)

(in-suite history-domain-tests)

(test pbt-history-exact-search-after-add-returns-entry
  "Exact search finds generated entries after they are added."
  (for-all-property (:trials 50) ((command (gen-shell-command)))
    (let ((history (history-with-lines command)))
      (let ((results (nshell.domain.history:history-search history command :mode :exact)))
        (is (some (lambda (entry)
                    (string= command (nshell.domain.history:entry-text entry)))
                  results)
            "Exact search should return generated command ~s" command)))))

(test pbt-history-prefix-results-match-prefix
  "Every prefix search result starts with the generated query prefix."
  (for-all-property (:trials 50)
      ((command (gen-shell-command))
       (prefix-length (gen-in-range 0 12)))
    (let* ((query (subseq command 0 (min prefix-length (length command))))
           (history (history-with-lines command
                                        (concatenate 'string command "-suffix")))
           (results (nshell.domain.history:history-search history query :mode :prefix)))
      (is (every (lambda (entry)
                   (nshell.domain.history::history-match-prefix entry query))
                 results)
          "Prefix search for ~s returned a non-matching entry" query))))

(test pbt-history-line-prefix-finds-generated-continuation-line
  "Line-prefix search finds generated prefixes after a newline."
  (for-all-property (:trials 50)
      ((prefix (gen-shell-word :min-length 1 :max-length 8))
       (suffix (gen-shell-word :min-length 1 :max-length 8)))
    (let* ((entry (format nil "echo setup~%~a-~a" prefix suffix))
           (history (history-with-lines entry))
           (results (nshell.domain.history:history-search history prefix
                                                          :mode :line-prefix)))
      (is (some (lambda (result)
                  (string= entry (nshell.domain.history:entry-text result)))
                results)
          "Line-prefix search should find generated continuation line ~s" entry))))

(test pbt-history-line-prefix-results-start-a-line-with-query
  "Every line-prefix result has at least one line starting with the query."
  (for-all-property (:trials 50)
      ((prefix (gen-shell-word :min-length 1 :max-length 8))
       (suffix (gen-shell-word :min-length 1 :max-length 8)))
    (let* ((matching (format nil "echo setup~%~a-~a" prefix suffix))
           (non-matching (format nil "echo setup~%pre-~a-~a" prefix suffix))
           (history (history-with-lines matching non-matching))
           (results (nshell.domain.history:history-search history prefix
                                                          :mode :line-prefix)))
      (is (every (lambda (entry)
                   (nshell.domain.history::history-match-line-prefix entry prefix))
                 results)
          "Line-prefix search for ~s returned a non-matching entry" prefix))))

(test pbt-history-dedup-after-duplicate-add-is-idempotent
  "Adding duplicates then deduplicating repeatedly preserves the same history."
  (for-all-property (:trials 50) ((command (gen-shell-command)))
    (let ((history (history-with-lines command command)))
      (nshell.domain.history:history-dedup history)
      (let ((once (history-entry-texts history)))
        (nshell.domain.history:history-dedup history)
        (let ((twice (history-entry-texts history)))
          (is (equal once twice)
              "Repeated dedup should preserve generated history for ~s" command)
          (is (= 1 (nshell.domain.history:history-size history))
              "Duplicate generated command ~s should appear once after dedup" command))))))

(test pbt-history-delete-removes-exact-generated-command
  "Deleting a generated command removes exact matches and leaves other entries."
  (for-all-property (:trials 50) ((command (gen-shell-command)))
    (let* ((sentinel (format nil "__sentinel__ ~a" command))
           (history (history-with-lines command sentinel)))
      (is (= 1 (nshell.domain.history:history-delete history command))
          "Generated command ~s should be deleted once" command)
      (is (not (member command (history-entry-texts history) :test #'string=))
          "Deleted generated command ~s should not remain" command)
      (is (member sentinel (history-entry-texts history) :test #'string=)
          "Deleting ~s should leave sentinel history entry" command))))

(test pbt-command-line-last-argument-returns-final-generated-word
  "For generated simple commands, the final word is the insertable last argument."
  (for-all-property (:trials 50)
      ((command (gen-shell-command :min-words 2 :max-words 5)))
    (let* ((words (uiop:split-string command :separator " "))
           (expected (car (last words))))
      (is (string= expected
                   (nshell.domain.history:command-line-last-argument command))
          "Generated command ~s should expose last argument ~s" command expected))))

(test pbt-command-line-last-argument-preserves-logical-shell-word-source
  "Quoted and escaped fragments that form one shell word are returned together."
  (for-all-property (:trials 50)
      ((quoted (gen-shell-word :min-length 1 :max-length 8))
       (suffix (gen-shell-word :min-length 1 :max-length 8))
       (escaped-head (gen-shell-word :min-length 1 :max-length 8))
       (escaped-tail (gen-shell-word :min-length 1 :max-length 8)))
    (let ((quoted-word (format nil "\"~a\"~a" quoted suffix))
          (escaped-word (format nil "~a\\ ~a" escaped-head escaped-tail)))
      (is (string= quoted-word
                   (nshell.domain.history:command-line-last-argument
                    (format nil "echo ~a" quoted-word)))
          "Quoted logical word ~s should be preserved" quoted-word)
      (is (string= escaped-word
                   (nshell.domain.history:command-line-last-argument
                    (format nil "echo ~a" escaped-word)))
          "Escaped logical word ~s should be preserved" escaped-word))))

(test pbt-history-navigation-keeps-original-prefix
  "History navigation keeps the generated prefix even after the buffer changes."
  (for-all-property (:trials 50) ((prefix (gen-shell-word :max-length 8)))
    (let ((history (history-with-lines (format nil "~a-older" prefix)
                                       "~unrelated-command"
                                       (format nil "~a-newer" prefix))))
      (let ((newer (nshell.domain.history:history-previous history prefix)))
        (is (string= (format nil "~a-newer" prefix) newer)
            "First navigation should return newer generated match for ~s" prefix)
        (let ((older (nshell.domain.history:history-previous history newer)))
          (is (string= (format nil "~a-older" prefix) older)
              "Second navigation should keep original prefix ~s" prefix)
          (is (string= newer (nshell.domain.history:history-next history))
              "Next navigation should return to the newer generated match")
          (is (string= prefix (nshell.domain.history:history-next history))
              "Newest boundary should restore original prefix ~s" prefix))))))

(test pbt-history-last-argument-at-follows-newest-first-order
  "Indexed last-argument lookup follows newest-first history order."
  (for-all-property (:trials 50)
      ((older (gen-shell-word :min-length 1 :max-length 8))
       (newer (gen-shell-word :min-length 1 :max-length 8)))
    (let ((history (history-with-lines (format nil "echo ~a" older)
                                       "pwd"
                                       (format nil "printf ~a" newer))))
      (is (string= newer
                   (nshell.domain.history:history-last-argument-at history 0)))
      (is (string= older
                   (nshell.domain.history:history-last-argument-at history 1)))
      (is (null (nshell.domain.history:history-last-argument-at history 2))))))

(test pbt-history-navigation-matches-generated-continuation-line-prefix
  "History navigation finds generated prefixes after a newline."
  (for-all-property (:trials 50)
      ((prefix (gen-shell-word :min-length 1 :max-length 8))
       (suffix (gen-shell-word :min-length 1 :max-length 8)))
    (let* ((multiline (format nil "echo setup~%~a-~a" prefix suffix))
           (history (history-with-lines multiline
                                        (format nil "printf 'not a prefix ~a'" prefix))))
      (is (string= multiline
                   (nshell.domain.history:history-previous history prefix))
          "Navigation should find continuation line prefix ~s" prefix)
      (is (string= prefix
                   (nshell.domain.history:history-next history))
          "Newest boundary should restore original prefix ~s" prefix))))
