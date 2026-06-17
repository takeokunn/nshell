(in-package #:nshell/test)

(in-suite input-state-tests)

(test input-state-alt-s-toggles-sudo-prefix
  (let ((state (input-state
                :buffer "apt update"
                :cursor-pos 3
                :completion-index 2
                :suggestion " && apt upgrade")))
    (with-expected-input-state-reduction (prefixed prefixed-output)
        state
        (reduce-once state :alt-s)
        :suggest-update
        (:buffer "sudo apt update"
         :cursor-pos 8
         :completion-index -1
         :suggestion nil)
      (with-expected-input-state-reduction (unprefixed unprefixed-output)
          prefixed
          (reduce-once prefixed :alt-s)
          :suggest-update
          (:buffer "apt update" :cursor-pos 3)))))

(test input-state-alt-s-removes-bare-sudo-prefix
  (let ((state (input-state
                :buffer "sudo"
                :cursor-pos 4)))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :alt-s)
        :suggest-update
        (:buffer "" :cursor-pos 0))))

(test input-state-ctrl-p-and-ctrl-n-request-history-navigation
  (let ((state (input-state
                :buffer "git"
                :cursor-pos 2
                :completion-index 1
                :suggestion " status")))
    (with-expected-input-state-reduction (prev-state prev-output)
        state
        (reduce-once state :ctrl-p)
        :history-prev
        (:buffer "git"
         :cursor-pos 2
         :completion-index 1
         :suggestion " status"))
    (with-expected-input-state-reduction (next-state next-output)
        state
        (reduce-once state :ctrl-n)
        :history-next
        (:buffer "git" :cursor-pos 2))))

(test input-state-alt-dot-requests-last-history-argument
  (let ((state (input-state
                :buffer "echo "
                :cursor-pos 5
                :completion-index 1
                :suggestion "tail")))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :alt-dot)
        :insert-last-argument
        (:buffer "echo "
         :cursor-pos 5
         :completion-index 1
         :suggestion "tail"))))

(test input-state-enter-on-text-returns-execute
  (let ((state (input-state :buffer "echo hi" :cursor-pos 7)))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :enter)
        :execute
        (:buffer "echo hi"))))

(test input-state-enter-accepts-suggestion-at-eol-before-execute
  (let ((state (input-state
                :buffer "echo"
                :cursor-pos 4
                :completion-index 1
                :suggestion " hello")))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :enter)
        :execute
        (:buffer "echo hello"
         :cursor-pos 10
         :completion-index -1
         :suggestion nil))))

(test input-state-enter-expands-abbreviation-before-execute
  (let ((state (input-state
                :buffer "gco"
                :cursor-pos 3
                :completion-index 2
                :suggestion " ignored"
                :abbreviation-expander
                (lambda (token)
                  (when (string= token "gco")
                    "git checkout")))))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :enter)
        :execute
        (:buffer "git checkout ignored"
         :cursor-pos 20
         :completion-index -1
         :suggestion nil))))

(test input-state-inserts-continuation-newline-at-cursor
  (let ((state (input-state :buffer "echo \"hi\"" :cursor-pos 5)))
    (with-expected-input-state-reduction (new-state output)
        state
        (nshell.presentation:insert-newline-at-cursor state)
        :suggest-update
        (:buffer (format nil "echo ~%\"hi\"")
         :cursor-pos 6))))

(test input-state-inserts-indented-continuation-newline-at-cursor
  (let ((state (input-state :buffer "echo |" :cursor-pos 6)))
    (with-expected-input-state-reduction (new-state output)
        state
        (nshell.presentation:insert-newline-at-cursor state :indent 2)
        :suggest-update
        (:buffer (format nil "echo |~%  ")
         :cursor-pos 9))))

(test input-state-ctrl-d-empty-quits-but-non-empty-deletes
  (multiple-value-bind (empty-state empty-output)
      (reduce-once (input-state) :ctrl-d)
    (declare (ignore empty-state))
    (is (eq :quit empty-output)))
  (let ((state (input-state :buffer "ab" :cursor-pos 1)))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :ctrl-d)
        :suggest-update
        (:buffer "a" :cursor-pos 1))))
