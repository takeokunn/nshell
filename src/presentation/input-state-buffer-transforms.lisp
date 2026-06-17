;;; Higher-level buffer transforms for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun sudo-prefixed-p (buffer)
  (or (string= buffer "sudo")
      (and (>= (length buffer) 5)
           (string= (subseq buffer 0 5) "sudo "))))

(defun toggle-sudo-prefix (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state)))
    (if (sudo-prefixed-p buffer)
        (let* ((remove-end (if (string= buffer "sudo") 4 5))
               (new-buffer (subseq buffer remove-end))
               (new-cursor (max 0 (- cursor remove-end))))
          (values (copy-input-state-clearing-completion state
                   :buffer new-buffer
                   :cursor-pos new-cursor)
                  :suggest-update))
        (values (copy-input-state-clearing-completion state
                 :buffer (concatenate 'string "sudo " buffer)
                 :cursor-pos (+ cursor 5))
                :suggest-update))))

(defun transpose-chars-around-cursor (state)
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state))
         (buffer-length (length buffer)))
    (if (or (< buffer-length 2) (zerop cursor))
        (values state :none)
        (let* ((left (if (= cursor buffer-length)
                         (- buffer-length 2)
                         (1- cursor)))
               (right (1+ left))
               (new-buffer (copy-seq buffer))
               (left-char (char new-buffer left)))
          (setf (char new-buffer left) (char new-buffer right)
                (char new-buffer right) left-char)
          (values (copy-input-state-clearing-completion state
                   :buffer new-buffer
                   :cursor-pos (if (= cursor buffer-length)
                                   buffer-length
                                   (1+ cursor)))
                  :suggest-update)))))
