(in-package #:nshell/test)

(in-suite input-state-tests)

(test pbt-input-state-history-search-result-selection-is-always-a-match
  "Applying generated search results always selects one result at the cursor end."
  (check-property (:trials 50)
      ((first-line (gen-prompt-text :min-length 1 :max-length 16)
                   #'shrink-prompt-text)
       (second-line (gen-prompt-text :min-length 1 :max-length 16)
                    #'shrink-prompt-text)
       (index (gen-in-range 0 32) nil))
    (let* ((results (list first-line second-line))
           (state (history-search-state
                   :query "q"
                   :original-buffer "original"
                   :index index))
           (applied
             (nshell.presentation:apply-history-search-results-to-input-state
              state results)))
      (and (member (nshell.presentation:input-state-buffer applied)
                   results
                   :test #'string=)
           (= (length (nshell.presentation:input-state-buffer applied))
              (nshell.presentation:input-state-cursor-pos applied))))))

(test pbt-input-state-history-search-ctrl-l-preserves-session-state
  "Ctrl-L in reverse search must only request a redraw and preserve the active search session."
  (check-property (:trials 50)
      ((selected (gen-prompt-text :min-length 0 :max-length 24)
                 #'shrink-prompt-text)
       (original (gen-prompt-text :min-length 0 :max-length 24)
                 #'shrink-prompt-text)
       (query (gen-prompt-text :min-length 1 :max-length 12)
              #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil)
       (search-index (gen-in-range 0 32) nil)
       (suggestion (gen-prompt-text :min-length 1 :max-length 12)
                   #'shrink-prompt-text)
       (candidate-a (gen-shell-word :min-length 1 :max-length 8)
                    #'shrink-prompt-text)
       (candidate-b (gen-shell-word :min-length 1 :max-length 8)
                    #'shrink-prompt-text)
       (candidate-c (gen-shell-word :min-length 1 :max-length 8)
                    #'shrink-prompt-text)
       (completion-index (gen-in-range 0 2) nil)
       (original-cursor-seed (gen-in-range 0 24) nil))
    (let* ((cursor (min cursor-seed (length selected)))
           (original-cursor (min original-cursor-seed (length original)))
           (state (history-search-state
                   :buffer selected
                   :cursor-pos cursor
                   :query query
                   :original-buffer original
                   :original-cursor original-cursor
                   :index search-index
                   :completion-index completion-index
                   :completion-base-buffer selected
                   :completion-base-cursor cursor
                   :last-candidates (list candidate-a candidate-b candidate-c)
                   :suggestion suggestion)))
      (multiple-value-bind (new-state output) (reduce-once state :ctrl-l)
        (and (eq :clear-screen output)
             (eq :search (nshell.presentation:input-state-mode new-state))
             (string= query (nshell.presentation:input-state-search-query new-state))
             (string= original
                      (nshell.presentation:input-state-search-original-buffer
                       new-state))
             (= original-cursor
                (nshell.presentation:input-state-search-original-cursor
                 new-state))
             (= search-index
                (nshell.presentation:input-state-search-index new-state))
             (string= selected (nshell.presentation:input-state-buffer new-state))
             (= cursor (nshell.presentation:input-state-cursor-pos new-state))
             (= completion-index
                (nshell.presentation:input-state-completion-index new-state))
             (string= selected
                      (nshell.presentation:input-state-completion-base-buffer
                       new-state))
             (= cursor
                (nshell.presentation:input-state-completion-base-cursor
                 new-state))
             (equal (list candidate-a candidate-b candidate-c)
                    (nshell.presentation:input-state-last-candidates
                     new-state))
             (equal suggestion
                    (nshell.presentation:input-state-suggestion new-state)))))))

(test pbt-input-state-history-search-accept-preserves-selected-buffer
  "Accepting a search result exits search mode without executing or restoring the original."
  (check-property (:trials 50)
      ((selected (gen-prompt-text :min-length 1 :max-length 24)
                 #'shrink-prompt-text)
       (original (gen-prompt-text :min-length 0 :max-length 12)
                 #'shrink-prompt-text))
    (let ((state (history-search-state
                  :buffer selected
                  :query "q"
                  :original-buffer original
                  :index 3)))
      (multiple-value-bind (accepted output)
          (nshell.presentation:reduce-input-state
           state
           (input-key-event :right))
        (and (eq :suggest-update output)
             (is-search-session-cleared accepted)
             (string= selected
                      (nshell.presentation:input-state-buffer accepted))
             (= (length selected)
                (nshell.presentation:input-state-cursor-pos accepted)))))))

(test pbt-input-state-history-search-empty-backspace-restores-original
  "Backspace on an empty reverse-search query behaves like cancel."
  (check-property (:trials 50)
      ((selected (gen-prompt-text :min-length 0 :max-length 24)
                 #'shrink-prompt-text)
       (original (gen-prompt-text :min-length 0 :max-length 24)
                 #'shrink-prompt-text)
       (index (gen-in-range 0 32) nil))
    (let ((state (history-search-state
                  :buffer selected
                  :query ""
                  :original-buffer original
                  :index index)))
      (multiple-value-bind (restored output)
          (nshell.presentation:reduce-input-state
           state
           (input-key-event :backspace))
        (and (eq :suggest-update output)
             (is-search-state restored
                              :mode :insert
                              :buffer original
                              :cursor-pos (length original)
                              :query ""
                              :original-buffer ""
                              :original-cursor nil
                              :index 0))))))

(test pbt-input-state-history-search-empty-results-restore-original-cursor
  "Empty history-search results restore the saved buffer and cursor."
  (check-property (:trials 50)
      ((selected (gen-prompt-text :min-length 0 :max-length 24)
                 #'shrink-prompt-text)
       (original (gen-prompt-text :min-length 0 :max-length 24)
                 #'shrink-prompt-text)
       (cursor (gen-in-range 0 32) nil)
       (index (gen-in-range 0 32) nil))
    (let* ((saved-cursor (min cursor (length original)))
           (state (history-search-state
                   :buffer selected
                   :query "q"
                   :original-buffer original
                   :original-cursor saved-cursor
                   :index index)))
        (let ((applied
                (nshell.presentation:apply-history-search-results-to-input-state
                 state '())))
          (and (eq :search (nshell.presentation:input-state-mode applied))
             (string= "q" (nshell.presentation:input-state-search-query applied))
             (string= original
                      (nshell.presentation:input-state-search-original-buffer
                       applied))
             (= saved-cursor
                (nshell.presentation:input-state-search-original-cursor applied))
             (= index (nshell.presentation:input-state-search-index applied))
             (string= original (nshell.presentation:input-state-buffer applied))
             (= saved-cursor
                (nshell.presentation:input-state-cursor-pos applied)))))))

(test pbt-input-state-history-search-paste-matches-typed-query
  "Bracketed paste in reverse search edits the query like typing the same text."
  (check-property (:trials 50)
      ((query (gen-prompt-text :min-length 1 :max-length 16)
              #'shrink-prompt-text)
       (original (gen-prompt-text :min-length 0 :max-length 12)
                 #'shrink-prompt-text))
    (let* ((base (history-search-state
                  :buffer original
                  :query ""
                  :original-buffer original
                  :index 4))
           (typed
             (loop with current = base
                   for ch across query
                   do (with-reduced-input-state (next-current)
                          (nshell.presentation:reduce-input-state
                           current
                           (input-key-event :char ch))
                        (setf current next-current))
                   finally (return current))))
      (multiple-value-bind (pasted output)
          (nshell.presentation:reduce-input-state
           base
           (input-key-event :paste nil nil
                            (list :protocol :bracketed :text query)))
        (and (eq :search-update output)
             (string= (nshell.presentation:input-state-search-query typed)
                      (nshell.presentation:input-state-search-query pasted))
             (string= original
                      (nshell.presentation:input-state-buffer pasted))
             (eq :search (nshell.presentation:input-state-mode pasted))
             (string= query
                      (nshell.presentation:input-state-search-query pasted))
             (string= original
                      (nshell.presentation:input-state-search-original-buffer
                       pasted))
             (= 0 (nshell.presentation:input-state-search-index pasted)))))))
