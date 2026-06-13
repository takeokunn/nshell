(in-package #:nshell/test)

(def-suite input-state-tests
  :description "Pure REPL input-state reducer tests"
  :in nshell-tests)

(in-suite input-state-tests)

(defun key (type &optional char number)
  (nshell.infrastructure.terminal:make-key-event type char number))

(defun reduce-once (state type &optional char number)
  (nshell.presentation:reduce-input-state state (key type char number)))

(test input-state-inserting-char-updates-buffer
  (multiple-value-bind (new-state output)
      (reduce-once (nshell.presentation:make-input-state) :char #\a)
    (is (string= "a" (nshell.presentation:input-state-buffer new-state)))
    (is (= 1 (nshell.presentation:input-state-cursor-pos new-state)))
    (is (eq :suggest-update output))))

(test input-state-cursor-moves-with-arrow-keys-within-bounds
  (let ((state (nshell.presentation:make-input-state :buffer "abc" :cursor-pos 1)))
    (multiple-value-bind (left-state left-output) (reduce-once state :left)
      (is (= 0 (nshell.presentation:input-state-cursor-pos left-state)))
      (is (eq :redraw left-output))
      (multiple-value-bind (bounded-left-state) (reduce-once left-state :left)
        (is (= 0 (nshell.presentation:input-state-cursor-pos bounded-left-state)))))
    (multiple-value-bind (right-state) (reduce-once state :right)
      (is (= 2 (nshell.presentation:input-state-cursor-pos right-state)))
      (multiple-value-bind (end-state) (reduce-once right-state :end)
        (is (= 3 (nshell.presentation:input-state-cursor-pos end-state)))
        (multiple-value-bind (bounded-right-state) (reduce-once end-state :right)
          (is (= 3 (nshell.presentation:input-state-cursor-pos bounded-right-state))))))))

(test input-state-backspace-removes-character-before-cursor
  (let ((state (nshell.presentation:make-input-state :buffer "abc" :cursor-pos 2)))
    (multiple-value-bind (new-state output) (reduce-once state :backspace)
      (is (string= "ac" (nshell.presentation:input-state-buffer new-state)))
      (is (= 1 (nshell.presentation:input-state-cursor-pos new-state)))
      (is (eq :suggest-update output)))))

(test input-state-enter-on-text-returns-execute
  (let ((state (nshell.presentation:make-input-state :buffer "echo hi" :cursor-pos 7)))
    (multiple-value-bind (new-state output) (reduce-once state :enter)
      (is (string= "echo hi" (nshell.presentation:input-state-buffer new-state)))
      (is (eq :execute output)))))

(test input-state-ctrl-d-empty-quits-but-non-empty-deletes
  (multiple-value-bind (empty-state empty-output)
      (reduce-once (nshell.presentation:make-input-state) :ctrl-d)
    (declare (ignore empty-state))
    (is (eq :quit empty-output)))
  (let ((state (nshell.presentation:make-input-state :buffer "ab" :cursor-pos 1)))
    (multiple-value-bind (new-state output) (reduce-once state :ctrl-d)
      (is (string= "a" (nshell.presentation:input-state-buffer new-state)))
      (is (= 1 (nshell.presentation:input-state-cursor-pos new-state)))
      (is (eq :suggest-update output)))))

(test input-state-tab-cycles-through-completion-candidates
  (let ((state (nshell.presentation:make-input-state
                :buffer "g"
                :cursor-pos 1
                :completion-index -1
                :last-candidates '("git" "grep" "go"))))
    (multiple-value-bind (first-state first-output) (reduce-once state :tab)
      (is (string= "git" (nshell.presentation:input-state-buffer first-state)))
      (is (= 0 (nshell.presentation:input-state-completion-index first-state)))
      (is (eq :complete first-output))
      (multiple-value-bind (second-state) (reduce-once first-state :tab)
        (is (string= "grep" (nshell.presentation:input-state-buffer second-state)))
        (is (= 1 (nshell.presentation:input-state-completion-index second-state)))
        (multiple-value-bind (reverse-state reverse-output) (reduce-once second-state :shift-tab)
          (is (string= "git" (nshell.presentation:input-state-buffer reverse-state)))
          (is (= 0 (nshell.presentation:input-state-completion-index reverse-state)))
          (is (eq :complete reverse-output)))))))

(test input-state-right-arrow-at-eol-accepts-suggestion
  (let ((state (nshell.presentation:make-input-state
                :buffer "git"
                :cursor-pos 3
                :suggestion " status")))
    (multiple-value-bind (new-state output) (reduce-once state :right)
      (is (string= "git status" (nshell.presentation:input-state-buffer new-state)))
      (is (= 10 (nshell.presentation:input-state-cursor-pos new-state)))
      (is (null (nshell.presentation:input-state-suggestion new-state)))
      (is (eq :suggest-update output)))))

(test input-state-ctrl-r-enters-search-mode
  (multiple-value-bind (new-state output)
      (reduce-once (nshell.presentation:make-input-state :buffer "abc" :cursor-pos 3)
                   :ctrl-r)
    (is (eq :search (nshell.presentation:input-state-mode new-state)))
    (is (eq :search-start output))))

(test input-state-ctrl-c-clears-buffer
  (let ((state (nshell.presentation:make-input-state
                :buffer "abc"
                :cursor-pos 2
                :completion-index 1
                :suggestion "def")))
    (multiple-value-bind (new-state output) (reduce-once state :ctrl-c)
      (is (string= "" (nshell.presentation:input-state-buffer new-state)))
      (is (= 0 (nshell.presentation:input-state-cursor-pos new-state)))
      (is (= -1 (nshell.presentation:input-state-completion-index new-state)))
      (is (null (nshell.presentation:input-state-suggestion new-state)))
      (is (eq :redraw output)))))

(test input-state-buffer-never-exceeds-reasonable-size
  (let* ((limit 4096)
         (buffer (make-string limit :initial-element #\x))
         (state (nshell.presentation:make-input-state :buffer buffer :cursor-pos limit)))
    (multiple-value-bind (new-state output) (reduce-once state :char #\y)
      (is (= limit (length (nshell.presentation:input-state-buffer new-state))))
      (is (string= buffer (nshell.presentation:input-state-buffer new-state)))
      (is (eq :none output)))))
