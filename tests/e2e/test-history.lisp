(in-package #:nshell/test)
(def-suite e2e-history-tests :description "E2E history tests" :in nshell-tests)
(in-suite e2e-history-tests)
(test e2e-history-persists-across-sessions
  (let ((h (nshell.domain.history:make-command-history :max-entries 10)))
    (nshell.domain.history:history-add h "cmd1")
    (nshell.domain.history:history-add h "cmd2")
    (is (= 2 (nshell.domain.history:history-size h)))
    (let ((results (nshell.domain.history:history-search h "cmd" :mode :prefix)))
      (is (= 2 (length results))))))

(test e2e-history-reverse-search-selects-and-executes-match
  (let ((history (nshell.domain.history:make-command-history :max-entries 10))
        (state (input-state)))
    (nshell.domain.history:history-add history "docker ps")
    (nshell.domain.history:history-add history "git status --short")
    (multiple-value-bind (search-state start-output)
        (nshell.presentation:reduce-input-state
         state
         (input-key-event :ctrl-r))
      (is (eq :search-start start-output))
      (setf state search-state))
    (dolist (ch (coerce "status" 'list))
      (setf state (reduce-once state :char ch)))
    (let* ((entries (nshell.application:search-history-use-case
                     history
                     (nshell.presentation:input-state-search-query state)
                     :contains))
           (texts (nshell.domain.history:history-entry-texts entries)))
      (setf state
            (nshell.presentation:apply-history-search-results-to-input-state
             state texts)))
    (is (string= "git status --short"
                 (nshell.presentation:input-state-buffer state)))
    (multiple-value-bind (finished output)
        (nshell.presentation:reduce-input-state
         state
         (input-key-event :enter))
      (is (eq :execute output))
      (is (eq :insert (nshell.presentation:input-state-mode finished)))
      (is (string= "git status --short"
                   (nshell.presentation:input-state-buffer finished))))))

(test e2e-history-reverse-search-start-does-not-preselect-history-before-query
  (with-repl-history-lines ("docker ps" "git status --short")
    (with-repl-input-state (:buffer "git" :cursor-pos 3)
      (multiple-value-bind (searching start-output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :ctrl-r))
        (is (eq :search-start start-output))
        (setf nshell.presentation::*input-state* searching)
        (capture-process-output-event start-output))
      (is (eq :search (nshell.presentation:input-state-mode
                       nshell.presentation::*input-state*)))
      (is (string= "git"
                   (nshell.presentation:input-state-buffer
                    nshell.presentation::*input-state*)))
      (is (= 3
             (nshell.presentation:input-state-cursor-pos
              nshell.presentation::*input-state*)))
      (is (string= ""
                   (nshell.presentation:input-state-search-query
                    nshell.presentation::*input-state*))))))

(test e2e-history-reverse-search-accepts-match-for-editing
  (let ((history (nshell.domain.history:make-command-history :max-entries 10))
        (state (input-state :buffer "git" :cursor-pos 3)))
    (nshell.domain.history:history-add history "docker ps")
    (nshell.domain.history:history-add history "git status --short")
    (multiple-value-bind (search-state start-output)
        (nshell.presentation:reduce-input-state
         state
         (input-key-event :ctrl-r))
      (is (eq :search-start start-output))
      (setf state search-state))
    (dolist (ch (coerce "status" 'list))
      (setf state (reduce-once state :char ch)))
    (let* ((entries (nshell.application:search-history-use-case
                     history
                     (nshell.presentation:input-state-search-query state)
                     :contains))
           (texts (nshell.domain.history:history-entry-texts entries)))
      (setf state
            (nshell.presentation:apply-history-search-results-to-input-state
             state texts)))
    (multiple-value-bind (accepted output)
        (nshell.presentation:reduce-input-state
         state
         (input-key-event :right))
      (is (eq :suggest-update output))
      (is (eq :insert (nshell.presentation:input-state-mode accepted)))
      (is (string= "git status --short"
                   (nshell.presentation:input-state-buffer accepted)))
      (multiple-value-bind (edited edit-output)
          (nshell.presentation:reduce-input-state
           accepted
           (input-key-event :char #\!))
        (is (eq :suggest-update edit-output))
        (is (string= "git status --short!"
                     (nshell.presentation:input-state-buffer edited)))))))

(test e2e-history-reverse-search-accepts-bracketed-paste-query
  (with-repl-history-lines ("docker ps" "git status --short")
    (with-repl-input-state (:buffer "git" :cursor-pos 3)
      (multiple-value-bind (searching start-output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :ctrl-r))
        (is (eq :search-start start-output))
        (setf nshell.presentation::*input-state* searching)
        (capture-process-output-event start-output))
      (multiple-value-bind (updated output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :paste nil nil
                            '(:protocol :bracketed :text "status --short")))
        (is (eq :search-update output))
        (setf nshell.presentation::*input-state* updated)
        (capture-process-output-event output))
      (is (string= "status --short"
                   (nshell.presentation:input-state-search-query
                    nshell.presentation::*input-state*)))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "git status --short"
                      :cursor-pos 18))))

(test e2e-history-reverse-search-prefers-continuation-line-prefix
  (with-repl-history-lines ("echo setup
git status" "printf 'not a prefix git'")
    (let ((multiline "echo setup
git status"))
      (with-repl-input-state ()
        (multiple-value-bind (searching output)
            (nshell.presentation:reduce-input-state
             nshell.presentation::*input-state*
             (input-key-event :ctrl-r))
          (is (eq :search-start output))
          (setf nshell.presentation::*input-state* searching)
          (capture-process-output-event output))
        (dolist (ch (coerce "git" 'list))
          (multiple-value-bind (updated output)
              (nshell.presentation:reduce-input-state
               nshell.presentation::*input-state*
               (input-key-event :char ch))
            (is (eq :search-update output))
            (setf nshell.presentation::*input-state* updated)
            (capture-process-output-event output)))
        (is-input-state nshell.presentation::*input-state*
                        :buffer multiline
                        :cursor-pos (length multiline))))))

(test e2e-history-up-prefers-continuation-line-prefix
  (with-repl-history-lines ("echo setup
git status" "printf 'not a prefix git'")
    (let ((multiline "echo setup
git status"))
      (with-repl-input-state (:buffer "git" :cursor-pos 3)
        (multiple-value-bind (requested output)
            (nshell.presentation:reduce-input-state
             nshell.presentation::*input-state*
             (input-key-event :up))
          (is (eq :history-prev output))
          (setf nshell.presentation::*input-state* requested)
          (capture-process-output-event output))
        (is-input-state nshell.presentation::*input-state*
                        :buffer multiline
                        :cursor-pos (length multiline))))))

(test e2e-history-autosuggests-continuation-line-prefix
  (with-repl-history-lines ("echo setup
git status --short")
    (with-repl-input-state (:buffer "git st" :cursor-pos 6)
      (let ((suggestion (nshell.presentation:compute-suggestion
                         nshell.presentation::*history*
                         (nshell.presentation:input-state-buffer
                          nshell.presentation::*input-state*))))
        (is (string= "atus --short" suggestion)))
      (let ((with-suggestion
              (nshell.presentation::copy-input-state-with
               nshell.presentation::*input-state*
               :suggestion "atus --short")))
        (multiple-value-bind (accepted output)
            (nshell.presentation:reduce-input-state
             with-suggestion
             (input-key-event :right))
          (is (eq :suggest-update output))
          (is-input-state accepted
                          :buffer "git status --short"
                          :cursor-pos 18))))))

(test e2e-history-alt-dot-inserts-last-argument-for-editing
  (with-repl-history-lines ("git status --short")
    (with-repl-input-state (:buffer "echo " :cursor-pos 5)
      (multiple-value-bind (requested output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :alt-dot))
        (is (eq :insert-last-argument output))
        (is-input-state requested :buffer "echo " :cursor-pos 5)
        (setf nshell.presentation::*input-state* requested)
        (capture-process-output-event output)
        (is-input-state nshell.presentation::*input-state*
                        :buffer "echo --short"
                        :cursor-pos 12)))))

(test e2e-history-alt-dot-cycles-older-last-arguments
  (with-repl-history-lines ("docker compose up api" "git status --short")
    (with-repl-input-state (:buffer "echo " :cursor-pos 5)
      (multiple-value-bind (requested output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :alt-dot))
        (is (eq :insert-last-argument output))
        (setf nshell.presentation::*input-state* requested)
        (capture-process-output-event output))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "echo --short"
                      :cursor-pos 12)
      (multiple-value-bind (requested output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :alt-dot))
        (is (eq :insert-last-argument output))
        (setf nshell.presentation::*input-state* requested)
        (capture-process-output-event output))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "echo api"
                      :cursor-pos 8))))

(test e2e-history-edit-after-up-starts-a-new-prefix-navigation
  (with-repl-history-lines ("git commit" "grep needle" "git status")
    (with-repl-input-state (:buffer "git" :cursor-pos 3)
      (multiple-value-bind (requested output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :up))
        (is (eq :history-prev output))
        (setf nshell.presentation::*input-state* requested)
        (capture-process-output-event output))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "git status"
                      :cursor-pos 10)
      (multiple-value-bind (edited edit-output)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :char #\!))
        (is (eq :suggest-update edit-output))
        (setf nshell.presentation::*input-state* edited)
        (capture-process-output-event edit-output))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "git status!"
                      :cursor-pos 11)
      (multiple-value-bind (requested-again output-again)
          (nshell.presentation:reduce-input-state
           nshell.presentation::*input-state*
           (input-key-event :up))
        (is (eq :history-prev output-again))
        (setf nshell.presentation::*input-state* requested-again)
        (capture-process-output-event output-again))
      (is-input-state nshell.presentation::*input-state*
                      :buffer "git status!"
                      :cursor-pos 11))))
