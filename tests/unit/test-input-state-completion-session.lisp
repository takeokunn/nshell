(in-package #:nshell/test)

(in-suite input-state-tests)

(test input-state-edit-after-completion-list-clears-stale-candidates
  (let ((state (input-state
                :buffer "g"
                :cursor-pos 1
                :completion-index -1
                :last-candidates '("git" "grep"))))
    (multiple-value-bind (edited edit-output) (reduce-once state :char #\x)
      (is-input-state edited
                      :buffer "gx"
                      :cursor-pos 2)
      (is-completion-session-cleared edited)
      (is (eq :suggest-update edit-output))
      (multiple-value-bind (tabbed tab-output) (reduce-once edited :tab)
        (is-input-state tabbed
                        :buffer "gx")
        (is-completion-session-cleared tabbed)
        (is (eq :complete tab-output))))))

(test input-state-escape-clears-completion-session-without-editing
  (let ((state (input-state
                :buffer "g"
                :cursor-pos 1
                :completion-index 0
                :completion-base-buffer "g"
                :completion-base-cursor 1
                :last-candidates '("git" "grep")
                :suggestion "it status")))
    (multiple-value-bind (new-state output) (reduce-once state :escape)
      (is-input-state new-state
                      :buffer "g"
                      :cursor-pos 1)
      (is-completion-session-cleared new-state)
      (is (eq :redraw output)))))

(test input-state-ctrl-g-clears-completion-session-without-editing
  (let ((state (input-state
                :buffer "git"
                :cursor-pos 2
                :completion-index 1
                :completion-base-buffer "g"
                :completion-base-cursor 1
                :last-candidates '("git" "grep")
                :suggestion " status")))
    (multiple-value-bind (new-state output) (reduce-once state :ctrl-g)
      (is-input-state new-state
                      :buffer "git"
                      :cursor-pos 2)
      (is-completion-session-cleared new-state)
      (is (eq :redraw output)))))

(test input-state-ctrl-c-clears-completion-session-on-empty-buffer
  (let ((state (input-state
                :buffer ""
                :cursor-pos 0
                :completion-index 0
                :completion-base-buffer ""
                :completion-base-cursor 0
                :last-candidates '("git"))))
    (multiple-value-bind (new-state output) (reduce-once state :ctrl-c)
      (is-input-state new-state
                      :buffer ""
                      :cursor-pos 0)
      (is-completion-session-cleared new-state)
      (is (eq :redraw output)))))

(test input-state-ctrl-l-preserves-completion-session
  (let ((state (input-state
                :buffer "g"
                :cursor-pos 1
                :completion-index 0
                :completion-base-buffer "g"
                :completion-base-cursor 1
                :last-candidates '("git" "grep"))))
    (multiple-value-bind (new-state output) (reduce-once state :ctrl-l)
      (is-input-state new-state
                      :buffer "g"
                      :cursor-pos 1
                      :completion-index 0
                      :completion-base-buffer "g"
                      :completion-base-cursor 1
                      :last-candidates '("git" "grep"))
      (is (eq :clear-screen output)))))

(test input-state-completion-session-key-predicates-return-canonical-booleans
  (is (eq t (nshell.presentation::key-preserves-yank-pop-p
             (input-key-event :ctrl-y))))
  (is (eq t (nshell.presentation::key-preserves-completion-session-p
             (input-key-event :tab))))
  (is (eq t (nshell.presentation::key-cancels-completion-session-p
             (input-key-event :escape))))
  (is (null (nshell.presentation::key-preserves-yank-pop-p
             (input-key-event :char #\a))))
  (is (null (nshell.presentation::key-preserves-completion-session-p
             (input-key-event :char #\a))))
  (is (null (nshell.presentation::key-cancels-completion-session-p
             (input-key-event :char #\a))))
  (let ((state (input-state
                :mode :insert
                :buffer "g"
                :cursor-pos 1
                :suggestion "it")))
    (is (eq t (nshell.presentation::completion-session-preserved-p
               state state (input-key-event :tab))))
    (is (null (nshell.presentation::completion-session-preserved-p
               state state (input-key-event :escape))))))
