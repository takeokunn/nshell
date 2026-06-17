;;; Undo and redo helpers for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun input-edit-snapshot (state)
  (list :buffer (input-state-buffer state)
        :cursor-pos (input-state-cursor-pos state)))

(defun input-edit-snapshot= (left right)
  (and (string= (getf left :buffer)
                (getf right :buffer))
       (= (getf left :cursor-pos)
          (getf right :cursor-pos))))

(defun input-state-edit-same-p (left right)
  (input-edit-snapshot= (input-edit-snapshot left)
                        (input-edit-snapshot right)))

(defun restore-input-edit-snapshot (state snapshot &key undo-stack redo-stack)
  (copy-input-state-clearing-completion state
                         :buffer (getf snapshot :buffer)
                         :cursor-pos (getf snapshot :cursor-pos)
                         :last-yank-start nil
                         :last-yank-end nil
                         :last-yank-index nil
                         :last-argument-start nil
                         :last-argument-end nil
                         :last-argument-index nil
                         :undo-stack undo-stack
                         :redo-stack redo-stack))

(defun undo-input-state (state)
  (let ((undo-stack (input-state-undo-stack state)))
    (if undo-stack
        (let ((current (input-edit-snapshot state))
              (previous (first undo-stack)))
          (values (restore-input-edit-snapshot
                   state
                   previous
                   :undo-stack (rest undo-stack)
                   :redo-stack (cons current (input-state-redo-stack state)))
                  :suggest-update))
        (values state :none))))

(defun redo-input-state (state)
  (let ((redo-stack (input-state-redo-stack state)))
    (if redo-stack
        (let ((current (input-edit-snapshot state))
              (next (first redo-stack)))
          (values (restore-input-edit-snapshot
                   state
                   next
                   :undo-stack (cons current (input-state-undo-stack state))
                   :redo-stack (rest redo-stack))
                  :suggest-update))
        (values state :none))))

(defun undo-recordable-transition-p (old-state new-state output key-event)
  (and (eq output :suggest-update)
       (eq (input-state-mode old-state) :insert)
       (eq (input-state-mode new-state) :insert)
       (not (member (key-event-type key-event)
                    '(:ctrl-underscore :alt-r)
                    :test #'eq))
       (not (input-state-edit-same-p old-state new-state))))

(defun record-undo-transition (old-state new-state output key-event)
  (if (undo-recordable-transition-p old-state new-state output key-event)
      (copy-input-state-with new-state
                             :undo-stack (cons (input-edit-snapshot old-state)
                                               (input-state-undo-stack
                                                new-state))
                             :redo-stack nil)
      new-state))
