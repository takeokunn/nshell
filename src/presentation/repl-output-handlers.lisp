;;; REPL output-event execution helpers
(in-package #:nshell.presentation)

(defun %continuation-indent-width (result)
  (if (or (nshell.domain.parsing:parse-diagnostic-kind-p
           result :trailing-continuation)
          (nshell.domain.parsing:parse-diagnostic-kind-p
           result :unclosed-block))
      2
      0))

(defun render-prompt-continuation ()
  (lambda () (render-prompt-cont)))

(defmacro with-reset-rendered-prompt-state-and-prompt-cont (&body body)
  `(progn
     ,@body
     (reset-rendered-prompt-state)
     (render-prompt-continuation)))

(defmacro with-cleared-rendered-completions-and-prompt-cont (&body body)
  `(with-reset-rendered-prompt-state-and-prompt-cont
     (clear-rendered-completions)
     ,@body))

(defun continue-multiline-input (result)
  (format t "~%")
  (reset-rendered-prompt-state)
  (multiple-value-bind (continued-state output)
      (insert-newline-at-cursor *input-state*
                                :indent (%continuation-indent-width result))
    (declare (ignore output))
    (setf *input-state* continued-state))
  (render-prompt-continuation))

(defun execute-parsed-input (text ast)
  (nshell.domain.history:history-add *history* text)
  (nshell.domain.history:history-reset-navigation *history*)
  (nshell.infrastructure.persistence:append-history-entry text)
  (sync-exported-environment)
  (setf *last-exit-code* (or (execute-ast ast) 0)))

(defun refresh-current-input-state-suggestion (&optional (text (input-state-buffer *input-state*)))
  (setf (input-state-suggestion *input-state*)
        (compute-suggestion *history*
                            text
                            :knowledge-base *kb*
                            :path (completion-path))))

(defun refresh-history-search-state ()
  (let* ((query (input-state-search-query *input-state*))
         (entries (nshell.application:interactive-history-search-use-case
                   *history* query))
         (texts (mapcar #'nshell.domain.history:entry-text entries)))
    (setf *input-state*
          (apply-history-search-results-to-input-state *input-state* texts))))

(defun %process-execute-output-event ()
  (clear-rendered-completions)
  (let ((text (input-state-buffer *input-state*)))
    (handler-case
        (if (string= text "")
            (with-reset-rendered-prompt-state-and-prompt-cont
              (format t "~%")
              (setf *input-state* (make-repl-input-state)))
            (nshell.domain.parsing:with-parsed-command-line-case (result ast text)
              (:complete
               (with-reset-rendered-prompt-state-and-prompt-cont
                 (format t "~%")
                 (execute-parsed-input text ast)
                 (setf *input-state* (make-repl-input-state))))
              (:error
               (with-reset-rendered-prompt-state-and-prompt-cont
                 (format t "~%")
                 (report-parse-diagnostics result *error-output*)
                 (setf *last-exit-code* 2
                       *input-state* (make-repl-input-state))))
              (:incomplete
               (continue-multiline-input result))))
      (error (condition)
        (with-reset-rendered-prompt-state-and-prompt-cont
          (format t "~%nshell error: ~a~%" condition)
          (setf *last-exit-code* 1
                *input-state* (make-repl-input-state)))))))

(defun %completion-cache-valid-p (state)
  (let ((candidates (input-state-last-candidates state))
        (selected-index (input-state-completion-index state)))
    (and candidates
         (>= selected-index 0)
         (< selected-index (length candidates))
         (let ((base-buffer (input-state-completion-base-buffer state))
               (base-cursor (input-state-completion-base-cursor state)))
           (and base-buffer
                base-cursor
                (multiple-value-bind (expected-buffer expected-cursor)
                    (apply-completion base-buffer
                                      (nth selected-index candidates)
                                      :cursor base-cursor)
                  (and (string= expected-buffer (input-state-buffer state))
                       (= expected-cursor (input-state-cursor-pos state)))))))))

(defun %process-complete-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont
    (let ((candidates (input-state-last-candidates *input-state*))
          (selected-index (input-state-completion-index *input-state*)))
      (if (%completion-cache-valid-p *input-state*)
          (setf *completion-rendered-lines*
                (%render-completions-below-prompt
                 candidates
                 :selected-index selected-index))
          (let* ((text (input-state-buffer *input-state*))
                 (candidates (when (> (length text) 0)
                               (nshell.domain.completion:complete
                                *kb* text :path (completion-path)))))
            (if candidates
                (progn
                  (multiple-value-bind (extended-state extended-p)
                      (maybe-extend-completion-common-prefix *input-state* candidates)
                    (declare (ignore extended-p))
                    (setf *input-state* extended-state))
                (setf (input-state-last-candidates *input-state*) candidates)
                (setf *completion-rendered-lines*
                        (%render-completions-below-prompt candidates)))
                (setf *input-state*
                      (clear-completion-session-state *input-state*))))))))

(defun %process-clear-screen-output-event ()
  (with-reset-rendered-prompt-state-and-prompt-cont
    (nshell.infrastructure.terminal:ansi-clear-screen)
    (nshell.infrastructure.terminal:ansi-move-cursor 1 1)
    (reset-rendered-completion-state)))

(defun %process-redraw-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont))

(defun %process-suggest-update-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont
    (nshell.domain.history:history-reset-navigation *history*)
    (refresh-current-input-state-suggestion)))

(defun %process-history-search-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont
    (refresh-history-search-state)))

(defun %process-history-prev-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont
    (let ((entry (nshell.domain.history:history-previous
                  *history*
                  (input-state-buffer *input-state*))))
      (when entry
        (setf *input-state*
              (make-repl-input-state :buffer entry :cursor-pos (length entry)))
        (refresh-current-input-state-suggestion)))))

(defun %process-history-next-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont
    (let ((entry (nshell.domain.history:history-next *history*)))
      (when entry
        (setf *input-state*
              (make-repl-input-state :buffer entry :cursor-pos (length entry)))
        (refresh-current-input-state-suggestion)))))

(defun %process-insert-last-argument-output-event ()
  (with-cleared-rendered-completions-and-prompt-cont
    (when (eq (insert-history-last-argument) :suggest-update)
      (refresh-current-input-state-suggestion))))

(defun %input-state-last-argument-active-span (state history)
  (let* ((buffer (input-state-buffer state))
         (start (input-state-last-argument-start state))
         (end (input-state-last-argument-end state))
         (index (input-state-last-argument-index state)))
    (when (and (integerp start)
               (integerp end)
               (integerp index)
               (<= 0 start end (length buffer)))
      (let ((argument
              (nshell.domain.history:history-last-argument-at history index)))
        (when (and argument
                   (string= argument (subseq buffer start end)))
          (values start end index))))))

(defun %replace-input-state-range (state start end replacement)
  (let* ((buffer (input-state-buffer state))
         (new-cursor (+ start (length replacement)))
         (new-buffer (concatenate 'string
                                  (subseq buffer 0 start)
                                  replacement
                                  (subseq buffer end))))
    (copy-input-state-clearing-completion state
                           :buffer new-buffer
                           :cursor-pos new-cursor
                           :last-argument-start start
                           :last-argument-end new-cursor)))

(defun %insert-history-last-argument-fresh (state argument)
  (let ((start (input-state-cursor-pos state)))
    (multiple-value-bind (new-state output)
        (insert-string-at-cursor state argument)
      (values (copy-input-state-with new-state
                                     :last-argument-start start
                                     :last-argument-end
                                     (input-state-cursor-pos new-state)
                                     :last-argument-index 0)
              output))))

(defun %cycle-history-last-argument (state history start end index)
  (let ((argument
          (nshell.domain.history:history-last-argument-at history (1+ index))))
    (when argument
      (values (copy-input-state-with
               (%replace-input-state-range state start end argument)
               :last-argument-index (1+ index))
              :suggest-update))))

(defun insert-history-last-argument ()
  (let ((old-state *input-state*))
    (multiple-value-bind (start end index)
        (%input-state-last-argument-active-span old-state *history*)
      (multiple-value-bind (new-state output)
          (if start
              (%cycle-history-last-argument old-state *history* start end index)
              (let ((argument
                      (nshell.domain.history:history-last-argument-at *history* 0)))
                (when argument
                  (%insert-history-last-argument-fresh old-state argument))))
        (when new-state
          (setf *input-state*
                (record-undo-transition
                 old-state new-state output
                 (nshell.infrastructure.terminal:make-key-event :alt-dot)))
          output)))))
