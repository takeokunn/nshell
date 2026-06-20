;;; Higher-level buffer transforms for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun toggle-sudo-prefix (state)
  (with-buffer-edit (state buffer cursor) state
    (if (or (string= buffer "sudo")
            (and (>= (length buffer) 5)
                 (string= (subseq buffer 0 5) "sudo ")))
        (let* ((remove-end (if (string= buffer "sudo") 4 5))
               (new-buffer (subseq buffer remove-end))
               (new-cursor (max 0 (- cursor remove-end))))
          (commit-buffer-edit new-buffer :cursor-pos new-cursor))
        (commit-buffer-edit (concatenate 'string "sudo " buffer)
                            :cursor-pos (+ cursor 5)))))

(defun transpose-chars-around-cursor (state)
  (with-buffer-edit (state buffer cursor) state
    (let ((buffer-length (length buffer)))
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
            (commit-buffer-edit new-buffer
                                :cursor-pos (if (= cursor buffer-length)
                                                buffer-length
                                                (1+ cursor))))))))
