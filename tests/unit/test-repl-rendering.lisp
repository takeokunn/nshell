(in-package #:nshell/test)

(in-suite repl-tests)

(test repl-non-completion-output-clears-rendered-completion-list
  "A stale completion menu should be erased before the next non-completion redraw."
  (with-repl-test-state
    (setf nshell.presentation::*config*
          (nshell.domain.configuration:default-config)
          nshell.presentation::*completion-rendered-lines* 2
          nshell.presentation::*prompt-rendered-lines* 3
          nshell.presentation::*prompt-rendered-cursor-row* 0)
    (with-repl-input-state (:buffer "g"
                            :cursor-pos 1
                            :completion-index 0)
      (let ((output (capture-process-output-event :suggest-update)))
        (is (= 0 nshell.presentation::*completion-rendered-lines*))
        (is (search (esc-sequence "[2K") output))
        (is (search (esc-sequence "[A") output))))))

(test repl-complete-with-no-candidates-clears-stale-completion-session
  "A failed completion attempt should not keep an old candidate list alive."
  (with-repl-test-state
    (with-repl-input-state (:buffer ""
                            :cursor-pos 0
                            :completion-index 0
                            :completion-base-buffer "g"
                            :completion-base-cursor 1
                            :last-candidates '("git" "grep"))
      (capture-process-output-event :complete)
      (is-input-state nshell.presentation::*input-state*
                      :buffer ""
                      :cursor-pos 0
                      :completion-index -1
                      :completion-base-buffer nil
                      :completion-base-cursor nil
                      :last-candidates nil
                      :suggestion nil))))

(test repl-completion-rendering-starts-below-current-prompt-row
  "Completion rendering should preserve the edit cursor and draw below all prompt rows."
  (with-repl-test-state
    (setf nshell.presentation::*config*
          (nshell.domain.configuration:default-config)
          nshell.presentation::*prompt-rendered-lines* 3
          nshell.presentation::*prompt-rendered-cursor-row* 0)
    (nshell.domain.completion:kb-add-command nshell.presentation::*kb*
                                             "git"
                                             :description "record changes")
    (nshell.domain.completion:kb-add-command nshell.presentation::*kb*
                                             "grep"
                                             :description "search text")
    (with-repl-input-state (:buffer "g"
                            :cursor-pos 1
                            :completion-index 0)
      (let ((output (capture-process-output-event :complete)))
        (is (search (esc-sequence "7") output))
        (is (search (format nil "~a~%" (esc-sequence "[2B")) output))
        (is (search "git" output))
        (is (search (esc-sequence "8") output))))))

(test repl-completion-tab-extends-unambiguous-common-prefix
  "Fresh completion should first advance the current token to the shared candidate prefix."
  (with-repl-test-state
    (nshell.domain.completion:kb-add-command nshell.presentation::*kb*
                                             "checkout"
                                             :description "switch branch")
    (nshell.domain.completion:kb-add-command nshell.presentation::*kb*
                                             "check-ignore"
                                             :description "debug ignores")
    (with-repl-input-state (:buffer "ch"
                            :cursor-pos 2)
      (let ((output (capture-process-output-event :complete)))
        (is-input-state nshell.presentation::*input-state*
                        :buffer "check"
                        :cursor-pos 5
                        :completion-index -1)
        (is (= 2
               (length (nshell.presentation:input-state-last-candidates
                        nshell.presentation::*input-state*))))
        (is (> nshell.presentation::*completion-rendered-lines* 0))
        (is (search "checkout" output))
        (is (search "check-ignore" output))))))

(test repl-completion-clear-restores-cursor-after-erasing-below-prompt
  "Completion clearing should erase the saved menu below the prompt without moving the edit cursor."
  (with-repl-test-state
    (setf nshell.presentation::*prompt-rendered-lines* 3
          nshell.presentation::*prompt-rendered-cursor-row* 0
          nshell.presentation::*completion-rendered-lines* 2)
    (let ((output
            (capture-standard-output
              (nshell.presentation::clear-rendered-completions))))
      (is (search (esc-sequence "7") output))
      (is (search (esc-sequence "[5B") output))
      (is (search (esc-sequence "[A") output))
      (is (search (esc-sequence "8") output))
      (is (= 0 nshell.presentation::*completion-rendered-lines*)))))

(test repl-clear-screen-clears-terminal-and-keeps-input-state
  "Ctrl-L should clear the terminal, reset render bookkeeping, and leave the edit session intact."
  (with-repl-test-state
    (setf nshell.presentation::*prompt-rendered-lines* 2
          nshell.presentation::*prompt-rendered-cursor-row* 1
          nshell.presentation::*completion-rendered-lines* 3)
    ;; Pin the prompt width and terminal size so the post-clear re-render
    ;; produces a deterministic single line regardless of the ambient working
    ;; directory (the default prompt renders the cwd, which is a long path in
    ;; the build sandbox and would otherwise wrap "git status" onto a 2nd row).
    (with-stable-repl-prompt (:width 4 :text "ns> ")
     (with-fixed-terminal-size (24 80)
      (with-repl-input-state (:buffer "git"
                            :cursor-pos 3
                            :completion-index 0
                            :completion-base-buffer "git"
                            :completion-base-cursor 3
                            :last-candidates '("git" "grep")
                            :suggestion " status")
        (let ((output
              (capture-process-output-event :clear-screen)))
        (is (search (esc-sequence "[2J") output))
        (is (search (esc-sequence "[1;1H") output))
        (is (= 1 nshell.presentation::*prompt-rendered-lines*))
        (is (= 0 nshell.presentation::*prompt-rendered-cursor-row*))
        (is (= 0 nshell.presentation::*completion-rendered-lines*))
        (is-input-state nshell.presentation::*input-state*
                        :buffer "git"
                        :cursor-pos 3
                        :completion-index 0
                        :completion-base-buffer "git"
                        :completion-base-cursor 3
                        :last-candidates '("git" "grep")
                        :suggestion " status")))))))

(test repl-insert-last-history-argument-updates-input-and-undo-stack
  "The REPL handler resolves Alt-dot against history and records a local undo point."
  (with-repl-history-lines ("git status --short")
    (with-repl-input-state (:buffer "echo " :cursor-pos 5)
      (capture-process-output-event :insert-last-argument)
      (is-input-state nshell.presentation::*input-state*
                      :buffer "echo --short"
                      :cursor-pos 12
                      :last-argument-start 5
                      :last-argument-end 12
                      :last-argument-index 0)
      (multiple-value-bind (undone output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :ctrl-underscore))
        (is (eq :suggest-update output))
        (is-input-state undone :buffer "echo " :cursor-pos 5)))))

(test repl-insert-last-history-argument-skips-leading-assignments
  "Alt-dot should ignore leading shell assignments before inserting the last argument."
  (with-repl-history-lines ("A=1 B=2 git status --short")
    (with-repl-input-state (:buffer "echo " :cursor-pos 5)
      (capture-process-output-event :insert-last-argument)
      (is-input-state nshell.presentation::*input-state*
                      :buffer "echo --short"
                      :cursor-pos 12
                      :last-argument-start 5
                      :last-argument-end 12
                      :last-argument-index 0))))

(test repl-insert-last-history-argument-preserves-escaped-space-arguments
  "Alt-dot should insert a logical shell word with escaped spaces unchanged."
  (with-repl-history-lines ("echo my\\ file.txt")
    (with-repl-input-state (:buffer "cp " :cursor-pos 3)
      (capture-process-output-event :insert-last-argument)
      (is-input-state nshell.presentation::*input-state*
                      :buffer "cp my\\ file.txt"
                      :cursor-pos (+ 3 (length "my\\ file.txt"))
                      :last-argument-start 3
                      :last-argument-end (+ 3 (length "my\\ file.txt"))
                      :last-argument-index 0)
      (multiple-value-bind (undone output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :ctrl-underscore))
        (is (eq :suggest-update output))
        (is-input-state undone :buffer "cp " :cursor-pos 3)))))

(test repl-insert-last-history-argument-cycles-older-arguments
  "Repeated Alt-dot replaces the previous insertion with older history arguments."
  (with-repl-history-lines ("echo older" "git status --short")
    (with-repl-input-state (:buffer "echo " :cursor-pos 5)
      (capture-process-output-event :insert-last-argument)
      (capture-process-output-event :insert-last-argument)
      (is-input-state nshell.presentation::*input-state*
                      :buffer "echo older"
                      :cursor-pos 10
                      :last-argument-start 5
                      :last-argument-end 10
                      :last-argument-index 1)
      (multiple-value-bind (undone output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :ctrl-underscore))
        (is (eq :suggest-update output))
        (is-input-state undone
                        :buffer "echo --short"
                        :cursor-pos 12
                        :last-argument-start nil
                        :last-argument-end nil
                        :last-argument-index nil)))))

(test repl-render-prompt-restores-midline-cursor-with-visible-width
  "Redraw should place the terminal cursor at the logical edit cursor, counting CJK width."
  (with-repl-test-state
    (with-stable-repl-prompt ()
      (with-fixed-terminal-size (24 80)
        (with-repl-render-state (:buffer "echo あbc"
                                 :cursor-pos 5
                                 :suggestion " --help")
          (let ((output (capture-standard-output
                          (nshell.presentation::render-prompt-cont))))
            (is (search (esc-sequence "[11D") output))))))))

(test repl-render-prompt-keeps-cursor-at-eol
  "Cursor-left rendering should be silent when the edit cursor is at the visible end."
  (is (= 0 (nshell.presentation::%cursor-tail-visible-width "echo あ" 6 nil nil)))
  (is (string= ""
               (capture-standard-output
                 (nshell.presentation::%move-cursor-to-rendered-position
                  "echo あ"
                  6
                  0
                  nil
                  nil)))))

(test repl-render-prompt-restores-cursor-across-continuation-lines
  "Multiline redraw should move up to the logical edit line and restore its absolute column."
  (let ((text (format nil "echo あ~%second")))
    (multiple-value-bind (row column)
        (nshell.presentation::%rendered-buffer-position text 6 7)
      (is (= 0 row))
      (is (= 14 column)))
    (let ((output (capture-standard-output
                    (nshell.presentation::%move-cursor-to-rendered-position
                     text
                     6
                     7
                     " --help"
                     nil))))
      (is (search (esc-sequence "[1A") output))
      (is (search (esc-sequence "[15G") output)))))

(test repl-rendered-position-wraps-at-terminal-width
  "Rendered cursor math should include terminal wrapping, not only logical newlines."
  (multiple-value-bind (row column)
      (nshell.presentation::%rendered-buffer-position "abcdefgh" 8 4
                                                      :terminal-width 10)
    (is (= 1 row))
    (is (= 2 column)))
  (multiple-value-bind (row column)
      (nshell.presentation::%rendered-buffer-position "あいうえ" 4 4
                                                      :terminal-width 10)
    (is (= 1 row))
    (is (= 2 column))))

(test repl-rendered-position-normalizes-long-prompt-width
  "Prompt width should contribute terminal-wrapped rows before input cursor math starts."
  (multiple-value-bind (row column)
      (nshell.presentation::%rendered-buffer-position "" 0 12
                                                      :terminal-width 10)
    (is (= 1 row))
    (is (= 2 column)))
  (multiple-value-bind (row column)
      (nshell.presentation::%rendered-buffer-position "abc" 3 12
                                                      :terminal-width 10)
    (is (= 1 row))
    (is (= 5 column)))
  (is (= 2
         (nshell.presentation::%rendered-buffer-line-count
          ""
          :terminal-width 10
          :prompt-width 12)))
  (is (= 3
         (nshell.presentation::%rendered-buffer-line-count
          "abcdefghi"
          :terminal-width 10
          :prompt-width 12))))

(test repl-rendered-line-count-includes-wrapped-suggestion
  "Prompt clearing should track terminal rows introduced by autosuggestion wrapping."
  (is (= 2
         (nshell.presentation::%rendered-buffer-line-count
          "abc"
          :suggestion "defgh"
          :terminal-width 10
          :prompt-width 4))))

(test repl-render-prompt-tracks-terminal-wrapped-lines
  "Prompt redraw state should include physical rows from terminal wrapping."
  (with-repl-test-state
    (with-stable-repl-prompt ()
      (with-repl-render-state (:buffer "abcdefg"
                               :cursor-pos 7
                               :suggestion "hi")
        (with-fixed-terminal-size (24 10)
          (capture-standard-output
            (nshell.presentation::render-prompt-cont))
          (is (= 2 nshell.presentation::*prompt-rendered-lines*))
          (is (= 1 nshell.presentation::*prompt-rendered-cursor-row*)))))))

(test repl-render-prompt-clears-stale-continuation-lines-before-redraw
  "A shorter redraw should erase continuation lines left by the previous prompt render."
  (with-repl-test-state
    (setf nshell.presentation::*prompt-rendered-lines* 3
          nshell.presentation::*prompt-rendered-cursor-row* 1)
    (let ((output (capture-standard-output
                    (nshell.presentation::clear-rendered-prompt))))
      (is (search (esc-sequence "[1B") output))
      (is (= 3
             (loop with needle = (esc-sequence "[2K")
                   for start = 0 then (+ position (length needle))
                   for position = (search needle output :start2 start)
                   while position
                   count position)))
      (is (= 0 nshell.presentation::*prompt-rendered-lines*))
      (is (= 0 nshell.presentation::*prompt-rendered-cursor-row*)))))

(test repl-render-prompt-tracks-multiline-render-state
  "Prompt redraw records enough state to clear a later redraw from the logical cursor row."
  (with-repl-test-state
    (with-stable-repl-prompt ()
      (with-repl-render-state (:buffer (format nil "one~%two")
                               :cursor-pos 2)
        (with-fixed-terminal-size (24 80)
          (capture-standard-output
            (nshell.presentation::render-prompt-cont))
          (is (= 2 nshell.presentation::*prompt-rendered-lines*))
          (is (= 0 nshell.presentation::*prompt-rendered-cursor-row*)))))))

(test repl-render-prompt-tracks-search-suffix-render-state
  "Search mode redraw should account for the rendered history suffix in both cursor math and line counts."
  (with-repl-test-state
    (with-stable-repl-prompt ()
      (with-fixed-terminal-size (24 10)
        (with-repl-render-state (:buffer "abc"
                                 :cursor-pos 3
                                 :mode :search
                                 :search-query "git"
                                 :search-original-buffer "abc"
                                 :search-original-cursor 3
                                 :search-index 0)
          (let ((output (capture-standard-output
                          (nshell.presentation::render-prompt-cont))))
            (is (search "history: git" output))
            (is (search (esc-sequence "[1A") output))
            (is (search (esc-sequence "[8G") output))
            (is (= 2 nshell.presentation::*prompt-rendered-lines*))
            (is (= 0 nshell.presentation::*prompt-rendered-cursor-row*))))))))

(test repl-rendered-position-includes-wrapped-suggestion-and-search-suffix
  "Cursor restoration should include both wrapped autosuggestion text and the history suffix."
  (is (= 3
         (nshell.presentation::%rendered-buffer-line-count
          "abc"
          :suggestion "defgh"
          :search-suffix " history: git"
          :terminal-width 10
          :prompt-width 0)))
  (is (string=
       (format nil "~C[2A~C[4G" #\Esc #\Esc)
       (capture-standard-output
         (nshell.presentation::%move-cursor-to-rendered-position
          "abc"
          3
          0
          "defgh"
          " history: git"
          :terminal-width 10)))))
