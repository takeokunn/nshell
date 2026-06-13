;;; Pure input state reducer for REPL line editing.

(in-package #:nshell.presentation)

(defconstant +max-input-buffer-size+ 4096
  "Maximum editable input buffer length accepted by `reduce-input-state'.")

(deftype input-mode ()
  "Input reducer modes."
  '(member :insert :search))

(deftype output-event ()
  "Events emitted by `reduce-input-state' for an outer, effectful REPL loop."
  '(member :redraw :execute :complete :suggest-update :search-start :search-update
    :history-prev :history-next :none :quit))

(defstruct (input-state (:constructor make-input-state
                                      (&key (buffer "")
                                            (cursor-pos 0)
                                            (completion-index -1)
                                            (last-candidates nil)
                                            suggestion
                                            (mode :insert))))
  "Pure line editor state.

BUFFER is the current editable text. CURSOR-POS is an index between 0 and
the buffer length. COMPLETION-INDEX and LAST-CANDIDATES model fish-style
completion cycling. SUGGESTION is the gray autosuggestion tail, if any.
MODE is either :INSERT or :SEARCH."
  (buffer "" :type string)
  (cursor-pos 0 :type integer)
  (completion-index -1 :type integer)
  (last-candidates nil :type list)
  (suggestion nil :type (or null string))
  (mode :insert :type input-mode))

(defun key-event-type (event)
  "Return EVENT's key type.

This forwards to the terminal key-event struct so presentation code can stay
independent from terminal decoding details while still exporting the accessor."
  (nshell.infrastructure.terminal:key-event-type event))

(defun key-event-char (event)
  "Return EVENT's character payload, if any."
  (nshell.infrastructure.terminal:key-event-char event))

(defun key-event-number (event)
  "Return EVENT's numeric payload, if any."
  (nshell.infrastructure.terminal:key-event-number event))

(defun clamp-cursor (position buffer)
  (max 0 (min position (length buffer))))

(defun normalize-input-state (state)
  (let* ((buffer (input-state-buffer state))
         (cursor (clamp-cursor (input-state-cursor-pos state) buffer)))
    (make-input-state :buffer buffer
                      :cursor-pos cursor
                      :completion-index (input-state-completion-index state)
                      :last-candidates (input-state-last-candidates state)
                      :suggestion (input-state-suggestion state)
                      :mode (input-state-mode state))))

(defun copy-input-state-with (state &key buffer cursor-pos completion-index
                                      last-candidates suggestion mode)
  (let* ((new-buffer (or buffer (input-state-buffer state)))
         (new-cursor (clamp-cursor (or cursor-pos (input-state-cursor-pos state))
                                   new-buffer)))
    (make-input-state :buffer new-buffer
                      :cursor-pos new-cursor
                      :completion-index (or completion-index
                                            (input-state-completion-index state))
                      :last-candidates (if last-candidates
                                           last-candidates
                                           (input-state-last-candidates state))
                      :suggestion (if (eq suggestion :clear)
                                      nil
                                      (if suggestion
                                         suggestion
                                         (input-state-suggestion state)))
                      :mode (or mode (input-state-mode state)))))

(defun insert-char-at-cursor (state ch)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state)))
    (if (>= (length buffer) +max-input-buffer-size+)
        (values state :none)
        (let ((new-buffer (concatenate 'string
                                       (subseq buffer 0 cursor)
                                       (string ch)
                                       (subseq buffer cursor))))
          (values (copy-input-state-with state
                                         :buffer new-buffer
                                         :cursor-pos (1+ cursor)
                                         :completion-index -1
                                         :suggestion :clear)
                  :suggest-update)))))

(defun backspace-before-cursor (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state)))
    (if (zerop cursor)
        (values state :none)
        (let ((new-buffer (concatenate 'string
                                       (subseq buffer 0 (1- cursor))
                                       (subseq buffer cursor))))
          (values (copy-input-state-with state
                                         :buffer new-buffer
                                         :cursor-pos (1- cursor)
                                         :completion-index -1
                                         :suggestion :clear)
                  :suggest-update)))))

(defun delete-char-at-cursor (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state)))
    (if (>= cursor (length buffer))
        (values state :none)
        (let ((new-buffer (concatenate 'string
                                       (subseq buffer 0 cursor)
                                       (subseq buffer (1+ cursor)))))
          (values (copy-input-state-with state
                                         :buffer new-buffer
                                         :completion-index -1
                                         :suggestion :clear)
                  :suggest-update)))))

(defun move-cursor (state delta)
  (let ((state (normalize-input-state state)))
    (values (copy-input-state-with state
                                   :cursor-pos (+ (input-state-cursor-pos state)
                                                  delta))
            :redraw)))

(defun move-cursor-to (state position)
  (let ((state (normalize-input-state state)))
    (values (copy-input-state-with state :cursor-pos position) :redraw)))

(defun accept-suggestion-at-eol (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (suggestion (input-state-suggestion state)))
    (if (and suggestion (= (input-state-cursor-pos state) (length buffer)))
        (let ((new-buffer (concatenate 'string buffer suggestion)))
          (values (copy-input-state-with state
                                         :buffer new-buffer
                                         :cursor-pos (length new-buffer)
                                         :suggestion :clear
                                         :completion-index -1)
                  :suggest-update))
        (move-cursor state 1))))

(defun cycle-completion-state (state direction)
  (let* ((state (normalize-input-state state))
         (candidates (input-state-last-candidates state)))
    (if (null candidates)
        (values state :complete)
        (let* ((count (length candidates))
               (index (mod (+ (input-state-completion-index state) direction)
                           count))
               (candidate (nth index candidates)))
          (values (copy-input-state-with state
                                         :buffer candidate
                                         :cursor-pos (length candidate)
                                         :completion-index index
                                         :suggestion :clear)
                  :complete)))))

(defun clear-input-state (state)
  (values (copy-input-state-with state
                                 :buffer ""
                                 :cursor-pos 0
                                 :completion-index -1
                                 :suggestion :clear
                                 :mode :insert)
          :redraw))

(defun kill-to-start (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state))
         (new-buffer (subseq buffer cursor)))
    (values (copy-input-state-with state
                                   :buffer new-buffer
                                   :cursor-pos 0
                                   :completion-index -1
                                   :suggestion :clear)
            :suggest-update)))

(defun kill-to-end (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state))
         (new-buffer (subseq buffer 0 cursor)))
    (values (copy-input-state-with state
                                   :buffer new-buffer
                                   :completion-index -1
                                   :suggestion :clear)
            :suggest-update)))

(defun reduce-input-state (state key-event)
  "Apply KEY-EVENT to INPUT-STATE and return two values.

The first value is a fresh INPUT-STATE. The second value is an OUTPUT-EVENT
keyword for the impure REPL shell to interpret. This function performs no I/O
and mutates neither STATE nor KEY-EVENT."
  (let ((state (normalize-input-state state)))
    (case (key-event-type key-event)
      (:char (let ((ch (key-event-char key-event)))
               (if ch
                   (insert-char-at-cursor state ch)
                   (values state :none))))
      (:enter (values state :execute))
      (:tab (cycle-completion-state state 1))
      (:shift-tab (cycle-completion-state state -1))
      (:backspace (backspace-before-cursor state))
      (:delete (delete-char-at-cursor state))
      (:ctrl-c (clear-input-state state))
      (:ctrl-d (if (string= "" (input-state-buffer state))
                   (values state :quit)
                   (delete-char-at-cursor state)))
      (:ctrl-r (values (copy-input-state-with state :mode :search)
                       :search-start))
      (:ctrl-f (accept-suggestion-at-eol state))
      (:ctrl-b (move-cursor state -1))
      (:ctrl-a (move-cursor-to state 0))
      (:ctrl-e (move-cursor-to state (length (input-state-buffer state))))
      (:ctrl-k (kill-to-end state))
      (:ctrl-u (kill-to-start state))
      (:ctrl-w (backspace-before-cursor state))
      (:left (move-cursor state -1))
      (:right (accept-suggestion-at-eol state))
      (:home (move-cursor-to state 0))
      (:end (move-cursor-to state (length (input-state-buffer state))))
      (:up (values state :history-prev))
      (:down (values state :history-next))
      ((:shift-up :shift-down :shift-left :shift-right) (values state :redraw))
      (:escape (values (if (eq (input-state-mode state) :search)
                           (copy-input-state-with state :mode :insert)
                           state)
                       :redraw))
      (otherwise (values state :none)))))
