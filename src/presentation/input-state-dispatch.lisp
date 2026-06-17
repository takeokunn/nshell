;;; Input-dispatch rules for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defmacro define-input-dispatcher (name state-var key-event-var &body clauses)
  `(defun ,name (,state-var ,key-event-var)
     (case (key-event-type ,key-event-var)
       ,@clauses
       (otherwise (values ,state-var :none)))))

(define-input-dispatcher reduce-insert-input-state state key-event
  (:char (let ((ch (key-event-char key-event)))
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
  (:ctrl-r (start-history-search state))
  (:ctrl-s (start-history-search state))
  (:ctrl-f (accept-suggestion-at-eol state))
  (:ctrl-b (move-cursor-clearing-suggestion state -1))
  (:ctrl-a (move-cursor-to-clearing-suggestion state 0))
  (:ctrl-e (accept-suggestion-or-move-end state))
  (:ctrl-k (kill-to-end state))
  (:ctrl-l (values state :clear-screen))
  (:ctrl-n (values state :history-next))
  (:ctrl-p (values state :history-prev))
  (:ctrl-t (transpose-chars-around-cursor state))
  (:ctrl-u (kill-to-start state))
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
  (:left (move-cursor-clearing-suggestion state -1))
  (:right (accept-suggestion-at-eol state))
  ((:alt-left :ctrl-left) (move-word-left state))
  ((:alt-right :ctrl-right) (accept-suggestion-word-at-eol state))
  (:alt-backspace (backward-kill-word state))
  (:alt-d (forward-kill-word state))
  (:alt-s (toggle-sudo-prefix state))
  (:home (move-cursor-to-clearing-suggestion state 0))
  (:end (accept-suggestion-or-move-end state))
  (:up (values state :history-prev))
  (:down (values state :history-next))
  (:ctrl-g (cancel-visible-suggestion state))
  ((:shift-up :shift-down :shift-left :shift-right
    :alt-up :alt-down :ctrl-up :ctrl-down
    :shift-alt-up :shift-alt-down :shift-alt-left :shift-alt-right
    :shift-ctrl-up :shift-ctrl-down :shift-ctrl-left :shift-ctrl-right
    :alt-ctrl-up :alt-ctrl-down :alt-ctrl-left :alt-ctrl-right
    :shift-alt-ctrl-up :shift-alt-ctrl-down :shift-alt-ctrl-left
    :shift-alt-ctrl-right
    :mouse)
   (values state :redraw))
  (:escape (cancel-visible-suggestion state)))
