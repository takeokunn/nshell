;;; History-search mode transitions for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun %history-search-state-buffer (state buffer cursor)
  (copy-input-state-clearing-completion state
                                        :buffer buffer
                                        :cursor-pos cursor
                                        :search-index (input-state-search-index state)))

(defun %history-search-original-state (state)
  (let ((original (input-state-search-original-buffer state)))
    (%history-search-state-buffer
     state
     original
     (or (input-state-search-original-cursor state)
         (length original)))))

(defun %apply-history-search-result-state (state matches)
  (let* ((index (mod (input-state-search-index state) (length matches)))
         (text (nth index matches)))
    (%history-search-state-buffer state text (length text))))

(defun %history-search-updated-state (state &rest args)
  (values (apply #'copy-input-state-clearing-completion state args)
          :search-update))

(defun %finish-history-search (state output)
  (values (copy-input-state-with
           (clear-history-search-session-state state)
           :mode :insert)
          output))

(defun %update-history-search-query (state text)
  (with-normalized-cleared-completion-state (state state)
    (let* ((query (input-state-search-query state))
           (remaining (- +max-input-buffer-size+ (length query))))
      (if (or (not (stringp text)) (zerop (length text)) (<= remaining 0))
          (values state :none)
          (let ((inserted (if (> (length text) remaining)
                              (subseq text 0 remaining)
                              text)))
            (%history-search-updated-state
             state
             :search-query (concatenate 'string query inserted)
             :search-index 0))))))

(defun %move-history-search-selection (state delta)
  (with-normalized-cleared-completion-state (state state)
    (%history-search-updated-state
     state
     :search-index (+ (input-state-search-index state) delta))))

(defun %backspace-history-search-query (state)
  (with-normalized-cleared-completion-state (state state)
    (let ((query (input-state-search-query state)))
      (if (zerop (length query))
          (cancel-history-search state)
          (%history-search-updated-state
           state
           :search-query (subseq query 0 (1- (length query)))
           :search-index 0)))))

(defun apply-history-search-results-to-input-state (state result-texts)
  "Apply history RESULT-TEXTS to STATE while preserving pure reducer semantics.

  RESULT-TEXTS must be strings, newest first. SEARCH-INDEX selects among them
with wraparound so repeated Ctrl-R can cycle through older matches."
  (with-normalized-cleared-completion-state (state state)
    (let ((matches (remove-if-not #'stringp result-texts)))
      (cond
        ((not (eq (input-state-mode state) :search))
         state)
        (matches
         (%apply-history-search-result-state state matches))
        (t
         (%history-search-original-state state))))))

(defun cancel-history-search (state)
  (with-normalized-cleared-completion-state (state state)
    (%finish-history-search (%history-search-original-state state)
                            :suggest-update)))

(defun reduce-search-input-state (state key-event)
  (case (nshell.domain.input:key-event-type key-event)
    (:char (let ((ch (nshell.domain.input:key-event-char key-event)))
             (if ch
                 (%update-history-search-query state (string ch))
                 (values state :none))))
    (:paste (%update-history-search-query
             state
             (getf (nshell.domain.input:key-event-data key-event)
                   :text)))
    (:backspace (%backspace-history-search-query state))
    ((:ctrl-r :up :ctrl-p) (%move-history-search-selection state 1))
    ((:ctrl-s :down :ctrl-n) (%move-history-search-selection state -1))
    (:enter (%finish-history-search state :execute))
    ((:right :ctrl-f) (%finish-history-search state :suggest-update))
    ((:escape :ctrl-g) (cancel-history-search state))
    (:ctrl-l (values state :clear-screen))
    (:ctrl-c (clear-input-state state))
    (otherwise (values state :redraw))))
