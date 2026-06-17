(in-package #:nshell/test)

(in-suite input-state-tests)

(test pbt-input-state-ctrl-l-preserves-buffer-and-cursor
  "Ctrl-L is a display request; it must not edit the current line."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor)))
      (with-expected-input-state-reduction (new-state output)
          state
          (reduce-once state :ctrl-l)
          :clear-screen
          (:buffer line
           :cursor-pos cursor)))))

(test pbt-input-state-ctrl-l-preserves-session-state
  "Ctrl-L should preserve completion and suggestion session state while clearing the screen."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil)
       (suggestion (gen-prompt-text :min-length 1 :max-length 12)
                   #'shrink-prompt-text)
       (candidate-a (gen-shell-word :min-length 1 :max-length 8)
                    #'shrink-prompt-text)
       (candidate-b (gen-shell-word :min-length 1 :max-length 8)
                    #'shrink-prompt-text)
       (candidate-c (gen-shell-word :min-length 1 :max-length 8)
                    #'shrink-prompt-text)
       (completion-index (gen-in-range 0 2) nil))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor
                   :completion-index completion-index
                   :completion-base-buffer line
                   :completion-base-cursor cursor
                   :last-candidates (list candidate-a candidate-b candidate-c)
                   :suggestion suggestion)))
      (with-expected-input-state-reduction (new-state output)
          state
          (reduce-once state :ctrl-l)
          :clear-screen
          (:buffer line
           :cursor-pos cursor
           :completion-index completion-index
           :completion-base-buffer line
           :completion-base-cursor cursor
           :last-candidates (list candidate-a candidate-b candidate-c)
           :suggestion suggestion)))))

(test pbt-input-state-word-navigation-respects-shell-token-boundaries
  "Word navigation treats escaped and quoted spaces as token content."
     (check-property (:trials 50)
         ((command (gen-shell-word :min-length 1 :max-length 8)
                   #'shrink-prompt-text)
          (left (gen-shell-word :min-length 1 :max-length 8)
                #'shrink-prompt-text)
          (right (gen-shell-word :min-length 1 :max-length 8)
                 #'shrink-prompt-text)
          (tail (gen-shell-word :min-length 1 :max-length 8)
                #'shrink-prompt-text))
       (let* ((escaped-token (format nil "~a\\ ~a" left right))
              (quoted-token (format nil "\"~a ~a\"" left right))
              (escaped-line (format nil "~a ~a ~a" command escaped-token tail))
              (quoted-line (format nil "~a ~a ~a" command quoted-token tail))
              (start (1+ (length command)))
              (escaped-next-start (+ start (length escaped-token) 1))
              (quoted-next-start (+ start (length quoted-token) 1))
              (escaped-state (input-state
                              :buffer escaped-line
                              :cursor-pos start))
              (quoted-state (input-state
                             :buffer quoted-line
                             :cursor-pos start)))
         (and (with-reduced-input-states escaped-state
                  (((right-state right-output) :alt-right)
                   ((left-state left-output) :alt-left))
                (and (eq :redraw right-output)
                     (eq :redraw left-output)
                     (string= escaped-line
                              (nshell.presentation:input-state-buffer right-state))
                     (string= escaped-line
                              (nshell.presentation:input-state-buffer left-state))
                     (= escaped-next-start
                        (nshell.presentation:input-state-cursor-pos right-state))
                     (= start
                        (nshell.presentation:input-state-cursor-pos left-state))))
              (with-reduced-input-states quoted-state
                  (((right-state right-output) :alt-right)
                   ((left-state left-output) :alt-left))
                (and (eq :redraw right-output)
                     (eq :redraw left-output)
                     (string= quoted-line
                              (nshell.presentation:input-state-buffer right-state))
                     (string= quoted-line
                              (nshell.presentation:input-state-buffer left-state))
                     (= quoted-next-start
                        (nshell.presentation:input-state-cursor-pos right-state))
                     (= start
                        (nshell.presentation:input-state-cursor-pos left-state))))))))

(test pbt-input-state-word-navigation-respects-shell-operator-boundaries
  "Word navigation skips shell operators as token separators."
  (check-property (:trials 50)
      ((left (gen-shell-word :min-length 1 :max-length 8)
             #'shrink-prompt-text)
       (right (gen-shell-word :min-length 1 :max-length 8)
              #'shrink-prompt-text)
       (operator-seed (gen-in-range 0 4) nil))
    (if (or (string= left "")
            (string= right ""))
        t
        (let* ((operators "|;&<>")
               (operator (char operators operator-seed))
               (line (format nil "~a~c~a" left operator right))
               (left-end (length left))
               (right-start (1+ left-end))
               (state (input-state
                       :buffer line
                       :cursor-pos 0))
               (operator-state (input-state
                                :buffer line
                                :cursor-pos left-end)))
          (with-reduced-input-states state
              (((right-start-state right-start-output) :alt-right)
               ((left-start-state left-start-output) :alt-left))
            (with-reduced-input-state (operator-right-state operator-right-output)
                (reduce-once operator-state :alt-right)
              (and (eq :redraw right-start-output)
                   (eq :redraw operator-right-output)
                   (eq :redraw left-start-output)
                   (= right-start
                      (nshell.presentation:input-state-cursor-pos right-start-state))
                   (= right-start
                      (nshell.presentation:input-state-cursor-pos operator-right-state))
                   (= 0
                      (nshell.presentation:input-state-cursor-pos left-start-state))
                   (string= line
                            (nshell.presentation:input-state-buffer left-start-state)))))))))

(test input-state-buffer-never-exceeds-reasonable-size
  (let* ((limit 4096)
         (buffer (make-string limit :initial-element #\x))
         (state (input-state :buffer buffer :cursor-pos limit)))
    (with-expected-input-state-reduction (new-state output)
        state
        (reduce-once state :char #\y)
        :none
        (:buffer buffer
         :cursor-pos limit))))

(test pbt-input-state-end-at-eol-accepts-entire-suggestion
  "End at the line tail accepts the complete autosuggestion suffix."
  (check-property (:trials 50)
      ((prefix (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (suffix (gen-prompt-text :min-length 1 :max-length 12)
               #'shrink-prompt-text))
    (let ((state (input-state
                  :buffer prefix
                  :cursor-pos (length prefix)
                  :suggestion suffix)))
      (with-reduced-input-state (new-state output) (reduce-once state :end)
        (let ((expected (concatenate 'string prefix suffix)))
          (and (eq :suggest-update output)
               (string= expected
                        (nshell.presentation:input-state-buffer new-state))
               (= (length expected)
                  (nshell.presentation:input-state-cursor-pos new-state))
               (null (nshell.presentation:input-state-suggestion new-state))))))))

(test pbt-input-state-ctrl-e-matches-end-for-suggestion-acceptance
  "Ctrl-E and End share line-end autosuggestion behavior."
  (check-property (:trials 50)
      ((prefix (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (suffix (gen-prompt-text :min-length 1 :max-length 12)
               #'shrink-prompt-text))
    (let ((state (input-state
                  :buffer prefix
                  :cursor-pos (length prefix)
                  :suggestion suffix)))
      (with-reduced-input-state (end-state end-output) (reduce-once state :end)
        (with-reduced-input-state (ctrl-e-state ctrl-e-output)
            (reduce-once state :ctrl-e)
          (and (eq end-output ctrl-e-output)
               (string= (nshell.presentation:input-state-buffer end-state)
                        (nshell.presentation:input-state-buffer ctrl-e-state))
               (= (nshell.presentation:input-state-cursor-pos end-state)
                  (nshell.presentation:input-state-cursor-pos ctrl-e-state))
               (equal (nshell.presentation:input-state-suggestion end-state)
                      (nshell.presentation:input-state-suggestion ctrl-e-state))))))))

(test pbt-input-state-cursor-navigation-clears-autosuggestion
  "Cursor navigation clears autosuggestion state without editing the buffer."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil)
       (suggestion (gen-prompt-text :min-length 1 :max-length 12)
                   #'shrink-prompt-text))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor
                   :suggestion suggestion)))
      (loop :for key :in '(:left :home :ctrl-b :ctrl-a)
            :always
            (with-reduced-input-state (new-state output)
                (reduce-once state key)
              (and (member output '(:suggest-update :none :redraw) :test #'eq)
                   (string= line (nshell.presentation:input-state-buffer new-state))
                   (null (nshell.presentation:input-state-suggestion new-state))
                   (<= 0 (nshell.presentation:input-state-cursor-pos new-state))
                   (<= (nshell.presentation:input-state-cursor-pos new-state)
                       (length line))))))))

(test pbt-input-state-right-and-ctrl-f-share-insert-mode-navigation
  "Right and Ctrl-F must stay aligned in insert mode across the EOL boundary."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil)
       (suggestion (gen-prompt-text :min-length 1 :max-length 12)
                   #'shrink-prompt-text))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor
                   :suggestion suggestion)))
      (with-reduced-input-state (right-state right-output)
          (reduce-once state :right)
        (with-reduced-input-state (ctrl-f-state ctrl-f-output)
            (reduce-once state :ctrl-f)
          (and (eq right-output ctrl-f-output)
               (string= (nshell.presentation:input-state-buffer right-state)
                        (nshell.presentation:input-state-buffer ctrl-f-state))
               (= (nshell.presentation:input-state-cursor-pos right-state)
                  (nshell.presentation:input-state-cursor-pos ctrl-f-state))
               (equal (nshell.presentation:input-state-suggestion right-state)
                      (nshell.presentation:input-state-suggestion ctrl-f-state))))))))

(test pbt-input-state-end-and-ctrl-e-only-accept-suggestion-at-eol
  "End and Ctrl-E preserve autosuggestions until the cursor is already at EOL."
  (check-property (:trials 50)
      ((line (gen-prompt-text :min-length 1 :max-length 24)
             #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil)
       (suggestion (gen-prompt-text :min-length 1 :max-length 12)
                   #'shrink-prompt-text))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor
                   :suggestion suggestion)))
      (with-reduced-input-state (end-state end-output) (reduce-once state :end)
        (with-reduced-input-state (ctrl-e-state ctrl-e-output)
            (reduce-once state :ctrl-e)
          (let ((at-eol (= cursor (length line))))
            (and (eq end-output ctrl-e-output)
                 (string= (nshell.presentation:input-state-buffer end-state)
                          (nshell.presentation:input-state-buffer ctrl-e-state))
                 (if at-eol
                     (and (= (length (concatenate 'string line suggestion))
                             (nshell.presentation:input-state-cursor-pos end-state))
                          (= (nshell.presentation:input-state-cursor-pos end-state)
                             (nshell.presentation:input-state-cursor-pos ctrl-e-state))
                          (null (nshell.presentation:input-state-suggestion end-state))
                          (null (nshell.presentation:input-state-suggestion ctrl-e-state))
                          (string= (concatenate 'string line suggestion)
                                   (nshell.presentation:input-state-buffer end-state)))
                     (and (= (length line)
                             (nshell.presentation:input-state-cursor-pos end-state))
                          (= (nshell.presentation:input-state-cursor-pos end-state)
                             (nshell.presentation:input-state-cursor-pos ctrl-e-state))
                          (string= line
                                   (nshell.presentation:input-state-buffer end-state))
                          (string= line
                                   (nshell.presentation:input-state-buffer ctrl-e-state))
                          (equal suggestion
                                 (nshell.presentation:input-state-suggestion end-state))
                          (equal suggestion
                                 (nshell.presentation:input-state-suggestion ctrl-e-state))))))))))

(test pbt-input-state-alt-right-accepts-compact-redirection
  "Autosuggestion word acceptance keeps compact redirections atomic."
  (check-property (:trials 50)
      ((prefix (gen-shell-command :min-words 1 :max-words 3
                                  :max-word-length 8)
               #'shrink-prompt-text)
       (fd (gen-in-range 0 9) nil)
       (target-fd (gen-in-range 0 9) nil)
       (target (gen-shell-word :min-length 1 :max-length 8)
               #'shrink-prompt-text)
       (style (gen-in-range 0 1) nil))
    (let* ((redirection
             (if (zerop style)
                 (format nil " ~d>&~d" fd target-fd)
                 (format nil " ~d>~a" fd target)))
           (tail " | cat")
           (suggestion (concatenate 'string redirection tail))
           (state (input-state
                   :buffer prefix
                   :cursor-pos (length prefix)
                   :suggestion suggestion)))
      (let ((expected (concatenate 'string prefix redirection)))
        (with-expected-input-state-reduction (new-state output)
            state
            (reduce-once state :alt-right)
            :suggest-update
            (:buffer expected
             :cursor-pos (length expected)
             :suggestion tail)))))

(test pbt-input-state-alt-s-twice-restores-buffer
  "Meta-S toggles a sudo command prefix without changing the command text after two presses."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24) #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil))
    (if (string= line "sudo")
        t
        (let* ((cursor (min cursor-seed (length line)))
               (state (input-state
                       :buffer line
                       :cursor-pos cursor)))
          (with-reduced-input-states state
              (((prefixed) :alt-s)
               ((restored) :alt-s))
            (string= line
                     (nshell.presentation:input-state-buffer restored))))))))

(test pbt-input-state-ctrl-t-preserves-length-and-characters
  "Transpose edits only character order and keeps the cursor within the line."
  (check-property (:trials 50)
      ((line (gen-prompt-text :min-length 2 :max-length 24
                              :cjk-probability 0.15)
             #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil))
    (let* ((cursor (max 1 (min cursor-seed (length line))))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor)))
      (with-reduced-input-state (new-state output)
          (reduce-once state :ctrl-t)
        (let ((new-line (nshell.presentation:input-state-buffer new-state)))
          (and (eq :suggest-update output)
               (= (length line) (length new-line))
               (string= (sort (copy-seq line) #'char<)
                        (sort (copy-seq new-line) #'char<))
               (<= 0 (nshell.presentation:input-state-cursor-pos new-state))
               (<= (nshell.presentation:input-state-cursor-pos new-state)
                   (length new-line))))))))

(test pbt-input-state-alt-t-preserves-length-and-characters
  "Word transpose reorders existing characters and keeps the cursor in bounds."
  (check-property (:trials 50)
      ((line (gen-shell-command :min-words 2 :max-words 5
                                :max-word-length 8)
             #'shrink-prompt-text))
    (let ((state (input-state
                  :buffer line
                  :cursor-pos (length line))))
      (with-reduced-input-state (new-state output)
          (reduce-once state :alt-t)
        (let ((new-line (nshell.presentation:input-state-buffer new-state)))
          (and (eq :suggest-update output)
               (= (length line) (length new-line))
               (string= (sort (copy-seq line) #'char<)
                        (sort (copy-seq new-line) #'char<))
               (<= 0 (nshell.presentation:input-state-cursor-pos new-state))
               (<= (nshell.presentation:input-state-cursor-pos new-state)
                   (length new-line))))))))

(test pbt-input-state-alt-case-preserves-length-and-cursor-bounds
  "Word case transforms keep the line shape stable and the cursor in bounds."
  (check-property (:trials 50)
      ((line (gen-shell-command :min-words 1 :max-words 5
                                :max-word-length 8)
             #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 64) nil))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor)))
      (loop :for key :in '(:alt-u :alt-l :alt-c)
            :always
            (with-reduced-input-state (new-state output)
                (reduce-once state key)
              (let ((new-line (nshell.presentation:input-state-buffer new-state))
                    (new-cursor
                      (nshell.presentation:input-state-cursor-pos new-state)))
                (and (member output '(:suggest-update :none) :test #'eq)
                     (= (length line) (length new-line))
                     (<= 0 new-cursor)
                     (<= new-cursor (length new-line)))))))))

(test pbt-input-state-undo-redo-roundtrips-typed-line
  "Undo walks typed edits back to an empty line; redo restores the line."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24
                              :cjk-probability 0.15)
             #'shrink-prompt-text))
    (let* ((events (map 'list
                        (lambda (ch)
                          (input-key-event :char ch))
                        line))
           (typed (apply-key-events-to-input-state (input-state) events))
           (undone typed))
      (dotimes (_ (length line))
        (setf undone (reduce-once-state undone :ctrl-underscore)))
      (let ((redone undone))
        (dotimes (_ (length line))
          (setf redone (reduce-once-state redone :alt-r)))
        (and (string= "" (nshell.presentation:input-state-buffer undone))
             (= 0 (nshell.presentation:input-state-cursor-pos undone))
             (string= line (nshell.presentation:input-state-buffer redone))
             (= (length line)
                (nshell.presentation:input-state-cursor-pos redone)))))))

(test pbt-terminal-control-h-matches-backspace-edit
  "ASCII BS decoded from terminal input behaves like the reducer backspace key."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24
                              :cjk-probability 0.15)
             #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor))
           (event (first (read-key-events-from-string
                          (string (code-char 8))))))
      (with-reduced-input-state (expected-state expected-output)
          (reduce-once state :backspace)
        (with-reduced-input-state (actual-state actual-output)
            (nshell.presentation:reduce-input-state state event)
          (and (eq expected-output actual-output)
               (string= (nshell.presentation:input-state-buffer expected-state)
                        (nshell.presentation:input-state-buffer actual-state))
               (= (nshell.presentation:input-state-cursor-pos expected-state)
                  (nshell.presentation:input-state-cursor-pos actual-state))))))))

(test pbt-terminal-ctrl-d-delete-or-quit-contract
  "Ctrl-D quits an empty prompt, otherwise it deletes the character under the cursor."
  (check-property (:trials 50)
      ((line (gen-prompt-text :max-length 24
                              :cjk-probability 0.15)
             #'shrink-prompt-text)
       (cursor-seed (gen-in-range 0 24) nil))
    (let* ((cursor (min cursor-seed (length line)))
           (state (input-state
                   :buffer line
                   :cursor-pos cursor))
           (event (first (read-key-events-from-string
                          (string (code-char 4))))))
      (with-reduced-input-state (new-state output)
          (nshell.presentation:reduce-input-state state event)
        (cond
          ((zerop (length line))
           (and (eq :quit output)
                (string= "" (nshell.presentation:input-state-buffer new-state))
                (= 0 (nshell.presentation:input-state-cursor-pos new-state))))
          ((< cursor (length line))
           (let ((expected (concatenate 'string
                                        (subseq line 0 cursor)
                                        (subseq line (1+ cursor)))))
             (and (eq :suggest-update output)
                  (string= expected
                           (nshell.presentation:input-state-buffer new-state))
                  (= cursor
                     (nshell.presentation:input-state-cursor-pos new-state)))))
          (t
           (and (eq :none output)
                 (string= line (nshell.presentation:input-state-buffer new-state))
                 (= cursor
                    (nshell.presentation:input-state-cursor-pos new-state))))))))))
