;;; Input-dispatch rules for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun reduce-insert-input-state (state key-event)
  (case (nshell.domain.input:key-event-type key-event)
    (:char (let ((ch (nshell.domain.input:key-event-char key-event)))
             (if ch
                 (insert-char-with-abbreviation-expansion state ch)
                 (values state :none))))
    (:paste (insert-paste-at-cursor state key-event))
    (:enter (finalize-enter-input-state state))
    (:tab (cycle-completion-state state 1))
    (:shift-tab (cycle-completion-state state -1))
    (:backspace (backspace-before-cursor state))
    (:delete (delete-char-at-cursor state))
    (:ctrl-c (clear-input-state state))
    (:ctrl-d (if (string= "" (input-state-buffer state))
                 (values state :quit)
                 (delete-char-at-cursor state)))
    ((:ctrl-r :ctrl-s)
     (with-normalized-cleared-completion-state (state state)
       (values (copy-input-state-clearing-completion
                state
                :mode :search
                :search-query ""
                :search-original-buffer (input-state-buffer state)
                :search-original-cursor (input-state-cursor-pos state)
                :search-index 0)
               :search-start)))
    ((:ctrl-f :right) (accept-suggestion-at-eol state))
    (:escape (if *vi-mode-enabled*
                 (values (vi-enter-command-mode state) :redraw)
                 (values (clear-completion-session-state state) :redraw)))
    (:ctrl-g (values (clear-completion-session-state state) :redraw))
    ((:ctrl-b :left) (move-cursor-clearing-suggestion state -1))
    ((:ctrl-a :home) (move-cursor-to-clearing-suggestion state 0))
    ((:ctrl-e :end)
     (with-normalized-input-state (state state)
       (if (input-state-at-eol-p state)
           (accept-suggestion-at-eol state)
           (move-cursor-to state (length (input-state-buffer state))))))
    (:ctrl-k (with-normalized-cleared-completion-state (state state)
               (%kill-range state
                            (input-state-cursor-pos state)
                            (length (input-state-buffer state))
                            (input-state-cursor-pos state))))
    (:ctrl-l (values state :clear-screen))
    ((:ctrl-n :down) (values state :history-next))
    ((:ctrl-p :up) (values state :history-prev))
    (:ctrl-t (transpose-chars-around-cursor state))
    (:ctrl-u (with-normalized-cleared-completion-state (state state)
               (%kill-range state
                            0
                            (input-state-cursor-pos state)
                            0)))
    (:ctrl-w (backward-kill-word state))
    (:ctrl-y (yank-last-kill state))
    (:ctrl-underscore (undo-input-state state))
    (:alt-r (redo-input-state state))
    (:alt-dot (values state :insert-last-argument))
    (:alt-c (capitalize-word-at-cursor state))
    (:alt-l (downcase-word-at-cursor state))
    (:alt-t (transpose-words-around-cursor state))
    (:alt-u (upcase-word-at-cursor state))
    (:alt-y (cycle-last-yank state))
    ((:alt-left :ctrl-left :alt-b) (move-word-left state))
    ((:alt-right :ctrl-right :alt-f) (accept-suggestion-word-at-eol state))
    (:alt-backspace (backward-kill-word state))
    (:alt-d (forward-kill-word state))
    (:alt-s (toggle-sudo-prefix state))
    ((:shift-up :shift-down :shift-left :shift-right
      :alt-up :alt-down :ctrl-up :ctrl-down
      :shift-alt-up :shift-alt-down :shift-alt-left :shift-alt-right
      :shift-ctrl-up :shift-ctrl-down :shift-ctrl-left :shift-ctrl-right
      :alt-ctrl-up :alt-ctrl-down :alt-ctrl-left :alt-ctrl-right
      :shift-alt-ctrl-up :shift-alt-ctrl-down :shift-alt-ctrl-left
      :shift-alt-ctrl-right
      :mouse)
     (values state :redraw))
    (otherwise (values state :none))))
