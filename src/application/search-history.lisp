(in-package #:nshell.application)

(defun %history-suggestion-preferred-entry (matches)
  (or (find-if (lambda (entry)
                 (let ((exit-code (nshell.domain.history:entry-exit-code entry)))
                   (or (null exit-code)
                       (zerop exit-code))))
               matches)
      (first matches)))

(defun %interactive-history-search-matches (history query)
  (when (and query (not (nshell.domain.parsing:shell-input-blank-p
                         query
                         :include-return-p t)))
    (let* ((line-prefix-matches
             (nshell.domain.history:history-search history query :mode :line-prefix))
           (line-prefix-texts
             (mapcar #'nshell.domain.history:entry-text line-prefix-matches))
           (contains-matches
             (remove-if (lambda (entry)
                          (member (nshell.domain.history:entry-text entry)
                                  line-prefix-texts
                                  :test #'string=))
                        (nshell.domain.history:history-search history query
                                                             :mode :contains))))
      (append line-prefix-matches contains-matches))))

(defun history-suggestion (history input &optional dispatcher)
  (unless (nshell.domain.parsing:shell-input-blank-p input)
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-completion-triggered-event input)))
    (let ((matches (nshell.domain.history:history-search history input :mode :line-prefix)))
      (when matches
        (let* ((best (%history-suggestion-preferred-entry matches))
               (suffix
                 (nshell.domain.history:history-entry-line-prefix-suffix
                  best
                  input
                  :case-sensitive (some #'upper-case-p input))))
          (when (and suffix (< 0 (length suffix)))
            suffix))))))

(defun search-history-use-case (history query mode &optional dispatcher)
  (let ((matches (nshell.domain.history:history-search history query :mode mode)))
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-domain-event :history-searched)))
    matches))

(defun interactive-history-search-use-case (history query &optional dispatcher)
  "Search for interactive reverse search, preferring command-line starts.

Line-prefix matches make multi-line history feel command-aware: a continuation
line that starts with QUERY ranks before incidental mid-line substring matches,
while the contains fallback preserves the usual Ctrl-R substring search."
  (let ((matches (%interactive-history-search-matches history query)))
    (when dispatcher
      (publish-event dispatcher
                     (nshell.domain.events:make-domain-event :history-searched)))
    matches))
