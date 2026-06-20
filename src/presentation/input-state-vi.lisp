;;; Vi-style modal key bindings for the pure REPL input reducer.
;;;
;;; When *VI-MODE-ENABLED* is true, pressing ESC in insert mode switches to vi
;;; normal (command) mode. Normal mode supports the common motions and edits;
;;; the operators d and c are modeled as transient modes (:vi-d / :vi-c) so no
;;; extra state field is required on INPUT-STATE. All functions here are pure.

(in-package #:nshell.presentation)

;; *VI-MODE-ENABLED* is declared in input-state-core so the dispatch table can
;; reference it before this file loads.

(defun %vi-buffer-length (state)
  (length (input-state-buffer state)))

(defun %vi-last-column (state)
  "Maximum cursor index in normal mode, where the cursor rests on a character."
  (max 0 (1- (%vi-buffer-length state))))

(defun vi-enter-command-mode (state)
  "Switch STATE to vi normal mode, moving the cursor left one as vi does on ESC."
  (copy-input-state-with (clear-completion-session-state state)
                         :mode :vi-command
                         :cursor-pos (max 0 (1- (input-state-cursor-pos state)))))

(defun %vi-enter-insert (state &optional position)
  (copy-input-state-with state
                         :mode :insert
                         :cursor-pos (or position (input-state-cursor-pos state))))

(defun %vi-kill-into (state start end cursor end-mode)
  "Kill the buffer span [START, END) (recording it on the kill ring), leave the
cursor at CURSOR, and switch to END-MODE (:vi-command for d, :insert for c)."
  (multiple-value-bind (killed-state output)
      (%kill-range state (max 0 (min start end)) (max start end) cursor)
    (declare (ignore output))
    (values (copy-input-state-with killed-state :mode end-mode) :redraw)))

(defun %reduce-vi-normal (state ch)
  "Handle a single character CH in vi normal mode."
  (let ((pos (input-state-cursor-pos state))
        (len (%vi-buffer-length state)))
    (case ch
      ;; Motions.
      ((#\h) (values (move-cursor-clearing-suggestion state -1) :redraw))
      ((#\l) (values (move-cursor-to-clearing-suggestion
                      state (min (%vi-last-column state) (1+ pos)))
                     :redraw))
      ((#\0) (values (move-cursor-to-clearing-suggestion state 0) :redraw))
      ((#\^) (values (move-cursor-to-clearing-suggestion state 0) :redraw))
      ((#\$) (values (move-cursor-to-clearing-suggestion state (%vi-last-column state))
                     :redraw))
      ((#\w) (values (move-word-right state) :redraw))
      ((#\b) (values (move-word-left state) :redraw))
      ((#\e) (values (move-cursor-to-clearing-suggestion
                      state (max pos (1- (next-kill-word-end (input-state-buffer state) pos))))
                     :redraw))
      ;; Enter insert mode.
      ((#\i) (values (%vi-enter-insert state) :redraw))
      ((#\a) (values (%vi-enter-insert state (min len (1+ pos))) :redraw))
      ((#\I) (values (%vi-enter-insert state 0) :redraw))
      ((#\A) (values (%vi-enter-insert state len) :redraw))
      ;; Single-key edits.
      ((#\x) (if (< pos len)
                 (%vi-kill-into state pos (1+ pos) pos :vi-command)
                 (values state :none)))
      ((#\D) (%vi-kill-into state pos len (max 0 (1- pos)) :vi-command))
      ((#\C) (%vi-kill-into state pos len pos :insert))
      ((#\s) (%vi-kill-into state pos (min len (1+ pos)) pos :insert))
      ;; Operators: remember via a transient mode.
      ((#\d) (values (copy-input-state-with state :mode :vi-d) :redraw))
      ((#\c) (values (copy-input-state-with state :mode :vi-c) :redraw))
      ;; History.
      ((#\j) (values state :history-next))
      ((#\k) (values state :history-prev))
      (otherwise (values state :none)))))

(defun %reduce-vi-operator (state ch op)
  "Apply operator OP (:d or :c) to the motion keyed by CH."
  (let* ((buffer (input-state-buffer state))
         (pos (input-state-cursor-pos state))
         (len (length buffer))
         (end-mode (if (eq op :c) :insert :vi-command))
         ;; The operator key repeated (dd / cc) acts on the whole line.
         (self (if (eq op :c) #\c #\d)))
    (cond
      ((char= ch self) (%vi-kill-into state 0 len 0 end-mode))
      ((char= ch #\w) (%vi-kill-into state pos (next-kill-word-end buffer pos) pos end-mode))
      ((char= ch #\b) (let ((start (previous-kill-word-start buffer pos)))
                        (%vi-kill-into state start pos start end-mode)))
      ((char= ch #\$) (%vi-kill-into state pos len (if (eq op :c) pos (max 0 (1- pos))) end-mode))
      ((char= ch #\0) (%vi-kill-into state 0 pos 0 end-mode))
      ;; Unknown motion cancels the pending operator.
      (t (values (copy-input-state-with state :mode :vi-command) :redraw)))))

(defun reduce-vi-input-state (state key-event)
  "Reduce KEY-EVENT while STATE is in one of the vi modes."
  (let ((type (nshell.domain.input:key-event-type key-event))
        (mode (input-state-mode state)))
    (case type
      (:enter (finalize-enter-input-state
               (copy-input-state-with state :mode :insert)))
      (:ctrl-c (clear-input-state (copy-input-state-with state :mode :insert)))
      (:ctrl-l (values state :clear-screen))
      ((:up) (values state :history-prev))
      ((:down) (values state :history-next))
      ((:left) (values (move-cursor-clearing-suggestion state -1) :redraw))
      ((:right) (values (move-cursor-to-clearing-suggestion
                         state (min (%vi-last-column state)
                                    (1+ (input-state-cursor-pos state))))
                        :redraw))
      (:char
       (let ((ch (nshell.domain.input:key-event-char key-event)))
         (cond
           ((null ch) (values state :none))
           ((eq mode :vi-d) (%reduce-vi-operator state ch :d))
           ((eq mode :vi-c) (%reduce-vi-operator state ch :c))
           (t (%reduce-vi-normal state ch)))))
      ;; ESC in an operator-pending mode cancels back to normal mode.
      (:escape (values (copy-input-state-with state :mode :vi-command) :redraw))
      (otherwise (values state :none)))))
