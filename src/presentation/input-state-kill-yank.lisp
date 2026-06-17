;;; Kill operations for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun %kill-range (state start end cursor-pos)
  (if (= start end)
      (values state :none)
      (let* ((buffer (input-state-buffer state))
             (killed (subseq buffer start end))
             (new-buffer (concatenate 'string
                                      (subseq buffer 0 start)
                                      (subseq buffer end))))
        (values (copy-input-state-clearing-completion state
                 :buffer new-buffer
                 :cursor-pos cursor-pos
                 :kill-ring (cons killed (input-state-kill-ring state)))
                :suggest-update))))

(defun kill-to-start (state)
  (let* ((state (normalize-input-state state))
         (cursor (input-state-cursor-pos state)))
    (%kill-range state 0 cursor 0)))

(defun kill-to-end (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state)))
    (%kill-range state cursor (length buffer) cursor)))

(defun backward-kill-word (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state))
         (start (previous-kill-word-start buffer cursor))
         (end cursor))
    (%kill-range state start end start)))

(defun forward-kill-word (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state))
         (end (next-kill-word-end buffer cursor))
         (start cursor))
    (%kill-range state start end cursor)))

(defun yank-last-kill (state)
  (let* ((state (normalize-input-state state))
         (killed (first (input-state-kill-ring state))))
    (if killed
        (let ((start (input-state-cursor-pos state)))
          (multiple-value-bind (new-state output)
              (insert-string-at-cursor state killed)
            (values (copy-input-state-with
                     new-state
                     :last-yank-start start
                     :last-yank-end (input-state-cursor-pos new-state)
                     :last-yank-index 0)
                    output)))
        (values state :none))))

(defun cycle-last-yank (state)
  (let* ((state (normalize-input-state state))
         (ring (input-state-kill-ring state))
         (buffer (input-state-buffer state))
         (start (input-state-last-yank-start state))
         (end (input-state-last-yank-end state))
         (index (input-state-last-yank-index state)))
    (if (and ring
             start
             end
             index
             (<= 0 start)
             (< start end)
             (<= end (length buffer))
             (= end (input-state-cursor-pos state))
             (< index (length ring))
             (string= (subseq buffer start end)
                      (nth index ring)))
        (let* ((next-index (mod (1+ index) (length ring)))
               (replacement (nth next-index ring))
               (new-buffer (concatenate 'string
                                        (subseq buffer 0 start)
                                        replacement
                                        (subseq buffer end)))
               (new-end (+ start (length replacement))))
          (values (copy-input-state-clearing-completion state
                   :buffer new-buffer
                   :cursor-pos new-end
                   :last-yank-start start
                   :last-yank-end new-end
                   :last-yank-index next-index)
                  :suggest-update))
        (values state :none))))
