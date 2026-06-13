(in-package #:nshell.application)
(defun history-suggestion (history input)
  (let ((matches (nshell.domain.history:history-search history input :mode :prefix)))
    (when matches
      (let ((best (first matches)))
        (subseq (nshell.domain.history:entry-text best) (length input))))))
(defun search-history-use-case (history query mode)
  (nshell.domain.history:history-search history query :mode mode))
