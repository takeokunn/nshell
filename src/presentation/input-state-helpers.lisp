;;; Small pure helpers for the REPL input reducer state.

(in-package #:nshell.presentation)

(defun clamp-cursor (position buffer)
  (max 0 (min position (length buffer))))

(defun copy-input-state-with (state &key buffer cursor-pos
                                      (completion-index nil
                                                        completion-index-supplied-p)
                                      (completion-base-buffer nil
                                                              completion-base-supplied-p)
                                      (completion-base-cursor nil
                                                              completion-base-cursor-supplied-p)
                                      (last-candidates nil
                                                       last-candidates-supplied-p)
                                      (suggestion nil suggestion-supplied-p)
                                      mode
                                      abbreviation-expander kill-ring
                                      (last-yank-start nil
                                                       last-yank-start-supplied-p)
                                      (last-yank-end nil
                                                     last-yank-end-supplied-p)
                                      (last-yank-index nil
                                                       last-yank-index-supplied-p)
                                      (last-argument-start nil
                                                           last-argument-start-supplied-p)
                                      (last-argument-end nil
                                                         last-argument-end-supplied-p)
                                      (last-argument-index nil
                                                           last-argument-index-supplied-p)
                                      search-query search-original-buffer
                                      search-original-cursor
                                      (search-index nil search-index-supplied-p)
                                      (undo-stack nil undo-stack-supplied-p)
                                      (redo-stack nil redo-stack-supplied-p))
  (let* ((new-buffer (or buffer (input-state-buffer state)))
         (new-cursor (clamp-cursor (or cursor-pos (input-state-cursor-pos state))
                                   new-buffer)))
    (make-input-state :buffer new-buffer
                      :cursor-pos new-cursor
                      :completion-index (if completion-index-supplied-p
                                            completion-index
                                            (input-state-completion-index state))
                      :completion-base-buffer
                      (cond
                        ((and completion-base-supplied-p
                              (eq completion-base-buffer :clear))
                         nil)
                        ((and completion-base-supplied-p
                              (stringp completion-base-buffer))
                         completion-base-buffer)
                        ((and completion-index-supplied-p
                              (= completion-index -1))
                         nil)
                        (t (input-state-completion-base-buffer state)))
                      :completion-base-cursor
                      (cond
                        ((and completion-base-cursor-supplied-p
                              (eq completion-base-cursor :clear))
                         nil)
                        ((and completion-base-cursor-supplied-p
                              (integerp completion-base-cursor))
                         completion-base-cursor)
                        ((and completion-index-supplied-p
                              (= completion-index -1))
                         nil)
                        (t (input-state-completion-base-cursor state)))
                      :last-candidates
                      (cond
                        ((and last-candidates-supplied-p
                              (eq last-candidates :clear))
                         nil)
                        (last-candidates-supplied-p last-candidates)
                        (t (input-state-last-candidates state)))
                      :suggestion (cond
                                    ((eq suggestion :clear) nil)
                                    (suggestion-supplied-p suggestion)
                                    (t (input-state-suggestion state)))
                      :mode (or mode (input-state-mode state))
                      :abbreviation-expander
                      (or abbreviation-expander
                          (input-state-abbreviation-expander state))
                      :kill-ring (cond
                                   ((eq kill-ring :clear) nil)
                                   (kill-ring kill-ring)
                                   (t (input-state-kill-ring state)))
                      :last-yank-start (if last-yank-start-supplied-p
                                           last-yank-start
                                           (input-state-last-yank-start state))
                      :last-yank-end (if last-yank-end-supplied-p
                                         last-yank-end
                                         (input-state-last-yank-end state))
                      :last-yank-index (if last-yank-index-supplied-p
                                           last-yank-index
                                           (input-state-last-yank-index state))
                      :last-argument-start
                      (if last-argument-start-supplied-p
                          last-argument-start
                          (input-state-last-argument-start state))
                      :last-argument-end
                      (if last-argument-end-supplied-p
                          last-argument-end
                          (input-state-last-argument-end state))
                      :last-argument-index
                      (if last-argument-index-supplied-p
                          last-argument-index
                          (input-state-last-argument-index state))
                      :search-query (cond
                                      ((eq search-query :clear) "")
                                      ((stringp search-query) search-query)
                                      (t (input-state-search-query state)))
                      :search-original-buffer
                      (cond
                        ((eq search-original-buffer :clear) "")
                        ((stringp search-original-buffer)
                         search-original-buffer)
                        (t (input-state-search-original-buffer state)))
                      :search-original-cursor
                      (cond
                        ((eq search-original-cursor :clear) nil)
                        ((integerp search-original-cursor)
                         search-original-cursor)
                        (t (input-state-search-original-cursor state)))
                      :search-index (if search-index-supplied-p
                                        search-index
                                        (input-state-search-index state))
                      :undo-stack (if undo-stack-supplied-p
                                      undo-stack
                                      (input-state-undo-stack state))
                      :redo-stack (if redo-stack-supplied-p
                                      redo-stack
                                      (input-state-redo-stack state)))))

(defun normalize-input-state (state)
  (copy-input-state-with
   state
   :buffer (input-state-buffer state)
   :cursor-pos (clamp-cursor (input-state-cursor-pos state)
                             (input-state-buffer state))))

(defun input-state-at-eol-p (state)
  (let ((state (normalize-input-state state)))
    (= (input-state-cursor-pos state)
       (length (input-state-buffer state)))))

(defun clear-completion-session-state (state)
  (copy-input-state-with state
                         :completion-index -1
                         :completion-base-buffer :clear
                         :completion-base-cursor :clear
                         :last-candidates :clear
                         :suggestion :clear))

(defun copy-input-state-clearing-completion (state &rest args)
  (apply #'copy-input-state-with
         (clear-completion-session-state state)
         args))

(defun expand-abbreviation-before-cursor (state)
  "Expand the token immediately before cursor if STATE has an expander."
  (let* ((state (normalize-input-state state))
         (expander (input-state-abbreviation-expander state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state)))
    (multiple-value-bind (new-buffer new-cursor expanded-p)
        (nshell.domain.abbreviation:expand-abbreviation
         buffer cursor expander :max-length +max-input-buffer-size+)
      (if (not expanded-p)
        (values state nil)
        (values (copy-input-state-clearing-completion state
                 :buffer new-buffer
                 :cursor-pos new-cursor)
                t)))))

(defun finalize-enter-input-state (state)
  (let* ((state (normalize-input-state state))
         (suggestion (and (input-state-at-eol-p state)
                          (input-state-suggestion state)))
         (state (expand-abbreviation-before-cursor state)))
    (when suggestion
      (setf state (append-suggestion-to-input-state state suggestion)))
    (values state :execute)))

(defun insert-char-with-abbreviation-expansion (state ch)
  (if (nshell.domain.abbreviation:abbreviation-boundary-p ch)
      (multiple-value-bind (expanded-state expanded-p)
          (expand-abbreviation-before-cursor state)
        (declare (ignore expanded-p))
        (insert-char-at-cursor expanded-state ch))
      (insert-char-at-cursor state ch)))
