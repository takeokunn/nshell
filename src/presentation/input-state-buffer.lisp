;;; Primitive buffer cursor and deletion operations for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defmacro with-normalized-input-state ((state-var state-form) &body body)
  `(let ((,state-var (normalize-input-state ,state-form)))
     ,@body))

(defmacro with-input-buffer ((state-var buffer-var cursor-var) state-form &body body)
  `(let* ((,state-var (normalize-input-state ,state-form))
          (,buffer-var (input-state-buffer ,state-var))
          (,cursor-var (input-state-cursor-pos ,state-var)))
     ,@body))

(defun %splice-buffer (buffer start end &optional inserted)
  (concatenate 'string
               (subseq buffer 0 start)
               inserted
               (subseq buffer end)))

(defun %commit-buffer-edit (state new-buffer &key cursor-pos)
  (values (copy-input-state-clearing-completion state
           :buffer new-buffer
           :cursor-pos (or cursor-pos (input-state-cursor-pos state)))
          :suggest-update))

(defun backspace-before-cursor (state)
  (with-input-buffer (state buffer cursor) state
    (if (zerop cursor)
        (values state :none)
        (%commit-buffer-edit state
                             (%splice-buffer buffer (1- cursor) cursor)
                             :cursor-pos (1- cursor)))))

(defun delete-char-at-cursor (state)
  (with-input-buffer (state buffer cursor) state
    (if (>= cursor (length buffer))
        (values state :none)
        (%commit-buffer-edit state
                             (%splice-buffer buffer cursor (1+ cursor))))))

(defun move-cursor-to (state position)
  (with-normalized-input-state (state state)
    (values (copy-input-state-with state :cursor-pos position) :redraw)))

(defun move-cursor-clearing-suggestion (state delta)
  (with-normalized-input-state (state state)
    (values (copy-input-state-with state
                                   :cursor-pos (+ (input-state-cursor-pos state)
                                                  delta)
                                   :suggestion :clear)
            :redraw)))

(defun move-cursor-to-clearing-suggestion (state position)
  (with-normalized-input-state (state state)
    (values (copy-input-state-with state
                                   :cursor-pos position
                                   :suggestion :clear)
            :redraw)))

(defun clear-input-state (state)
  (values (copy-input-state-clearing-completion state
                                 :buffer ""
                                 :cursor-pos 0
                                 :mode :insert
                                 :search-query :clear
                                 :search-original-buffer :clear
                                 :search-original-cursor :clear
                                 :search-index 0)
          :redraw))

(defun insert-char-at-cursor (state ch)
  (with-input-buffer (state buffer cursor) state
    (if (>= (length buffer) +max-input-buffer-size+)
        (values state :none)
        (%commit-buffer-edit state
                             (%splice-buffer buffer cursor cursor (string ch))
                             :cursor-pos (1+ cursor)))))

(defun insert-string-at-cursor (state text)
  "Insert TEXT at cursor, capped by `+max-input-buffer-size+'."
  (with-input-buffer (state buffer cursor) state
    (let ((remaining (- +max-input-buffer-size+ (length buffer))))
      (if (or (not (stringp text)) (<= remaining 0) (zerop (length text)))
          (values state :none)
          (let* ((inserted (if (> (length text) remaining)
                               (subseq text 0 remaining)
                               text)))
            (%commit-buffer-edit state
                                 (%splice-buffer buffer cursor cursor inserted)
                                 :cursor-pos (+ cursor (length inserted))))))))

(defun normalize-paste-text (text)
  "Normalize pasted line endings to LF while preserving other text."
  (when (stringp text)
    (with-output-to-string (stream)
      (loop with index = 0
            while (< index (length text))
            for ch = (char text index)
            do (cond
                 ((char= ch #\Return)
                  (write-char #\Newline stream)
                  (incf index)
                  (when (and (< index (length text))
                             (char= (char text index) #\Newline))
                    (incf index)))
                 (t
                  (write-char ch stream)
                  (incf index)))))))

(defun insert-paste-at-cursor (state event)
  (insert-string-at-cursor state
                           (normalize-paste-text
                            (getf (key-event-data event) :text))))

(defun insert-newline-at-cursor (state &key (indent 0))
  "Insert a logical continuation newline at the cursor."
  (let ((newline (concatenate 'string
                              (string #\Newline)
                              (make-string (max 0 indent)
                                           :initial-element #\Space))))
    (insert-string-at-cursor state newline)))
