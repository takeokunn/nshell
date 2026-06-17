;;; History-search mode transitions for the pure REPL input reducer.

(in-package #:nshell.presentation)

(defun %copy-input-state-clearing-history-search-selection (state &rest args)
  (apply #'copy-input-state-with
         (copy-input-state-clearing-completion state)
         args))

(defun append-history-search-query (state text)
  (let* ((state (normalize-input-state state))
         (query (input-state-search-query state))
         (remaining (- +max-input-buffer-size+ (length query))))
    (if (or (not (stringp text)) (zerop (length text)) (<= remaining 0))
        (values state :none)
        (let ((inserted (if (> (length text) remaining)
                            (subseq text 0 remaining)
                            text)))
          (values (%copy-input-state-clearing-history-search-selection
                   state
                   :search-query (concatenate 'string query inserted)
                   :search-index 0)
                  :search-update)))))

(defun backspace-history-search-query (state)
  (let* ((state (normalize-input-state state))
         (query (input-state-search-query state)))
    (if (zerop (length query))
        (cancel-history-search state)
        (values (%copy-input-state-clearing-history-search-selection
                 state
                 :search-query (subseq query 0 (1- (length query)))
                 :search-index 0)
                :search-update))))

(defun move-history-search-selection (state delta)
  (let ((state (normalize-input-state state)))
    (values (%copy-input-state-clearing-history-search-selection
             state
             :search-index (+ (input-state-search-index state) delta))
            :search-update)))

(defun %restore-original-history-search-state (state)
  (let ((original (input-state-search-original-buffer state)))
    (%copy-input-state-clearing-history-search-selection
     state
     :buffer original
     :cursor-pos (or (input-state-search-original-cursor state)
                     (length original))
     :search-index (input-state-search-index state))))

(defun %apply-history-search-result-state (state matches)
  (let* ((index (mod (input-state-search-index state) (length matches)))
         (text (nth index matches)))
    (%copy-input-state-clearing-history-search-selection
     state
     :buffer text
     :cursor-pos (length text)
     :search-index (input-state-search-index state))))

(defun apply-history-search-results-to-input-state (state result-texts)
  "Apply history RESULT-TEXTS to STATE while preserving pure reducer semantics.

RESULT-TEXTS must be strings, newest first. SEARCH-INDEX selects among them
with wraparound so repeated Ctrl-R can cycle through older matches."
  (let* ((state (normalize-input-state state))
         (matches (remove-if-not #'stringp result-texts)))
    (if (not (eq (input-state-mode state) :search))
        state
        (if matches
            (%apply-history-search-result-state state matches)
            (%restore-original-history-search-state state)))))

(defun start-history-search (state)
  (let ((state (normalize-input-state state)))
    (values (%copy-input-state-clearing-history-search-selection
             state
             :mode :search
             :search-query ""
             :search-original-buffer (input-state-buffer state)
             :search-original-cursor (input-state-cursor-pos state)
             :search-index 0)
            :search-start)))

(defun clear-history-search-session (state)
  (%copy-input-state-clearing-history-search-selection
   state
   :mode :insert
   :search-query :clear
   :search-original-buffer :clear
   :search-original-cursor :clear
   :search-index 0))

(defun cancel-history-search (state)
  (let* ((state (normalize-input-state state))
         (original (input-state-search-original-buffer state))
         (original-cursor (or (input-state-search-original-cursor state)
                              (length original))))
    (values (clear-history-search-session
             (copy-input-state-with state
                                    :buffer original
                                    :cursor-pos original-cursor))
            :suggest-update)))

(defun %end-history-search (state event)
  (values (clear-history-search-session state)
          event))

(defun finish-history-search (state)
  (%end-history-search state :execute))

(defun accept-history-search (state)
  (%end-history-search state :suggest-update))

(defun reduce-search-input-state (state key-event)
  (case (key-event-type key-event)
    (:char (let ((ch (key-event-char key-event)))
             (if ch
                 (append-history-search-query state (string ch))
                 (values state :none))))
    (:paste (append-history-search-query state (getf (key-event-data key-event) :text)))
    (:backspace (backspace-history-search-query state))
    ((:ctrl-r :up :ctrl-p) (move-history-search-selection state 1))
    ((:ctrl-s :down :ctrl-n) (move-history-search-selection state -1))
    (:enter (finish-history-search state))
    ((:right :ctrl-f) (accept-history-search state))
    ((:escape :ctrl-g) (cancel-history-search state))
    (:ctrl-l (values state :clear-screen))
    (:ctrl-c (clear-input-state state))
    (otherwise (values state :redraw))))
