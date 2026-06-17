(in-package #:nshell.domain.history)

(defun history-add (history text &optional exit-code)
  "Add TEXT to HISTORY and keep the newest entry for duplicate command text."
  (let* ((entry (make-history-entry text (get-universal-time) exit-code))
         (existing (remove-if (lambda (candidate)
                                (entry-equal-p candidate entry))
                              (command-history-entries history)))
         (new-entries (cons entry existing)))
    (setf (command-history-entries history)
          (subseq new-entries 0 (min (length new-entries)
                                     (command-history-max-entries history))))
    history))

(defun history-all (history)
  "Return all history entries, most recent first."
  (command-history-entries history))

(defun history-empty-p (history)
  "True if HISTORY has no entries."
  (null (command-history-entries history)))

(defun history-clear (history)
  "Remove all entries from HISTORY and reset transient navigation."
  (setf (command-history-entries history) nil)
  (%history-clear-navigation history))

(defun history-delete (history text &key (case-sensitive t))
  "Delete entries whose text exactly matches TEXT and return the deleted count."
  (let* ((old-entries (command-history-entries history))
         (new-entries
           (remove-if (lambda (entry)
                        (if case-sensitive
                            (string= text (entry-text entry))
                            (string-equal text (entry-text entry))))
                      old-entries))
         (deleted (- (length old-entries) (length new-entries))))
    (setf (command-history-entries history) new-entries)
    (when (plusp deleted)
      (%history-clear-navigation history))
    deleted))

(defun history-size (history)
  "Return current number of entries in HISTORY."
  (length (command-history-entries history)))

(defun history-dedup (history)
  "Remove duplicate entries from HISTORY, keeping the most recent entries."
  (let ((seen (make-hash-table :test #'equal)))
    (setf (command-history-entries history)
          (remove-if-not (lambda (entry)
                           (let ((text (history-entry-text entry)))
                             (unless (gethash text seen)
                               (setf (gethash text seen) t))))
                         (command-history-entries history)))
    history))

(defun history-merge (history entries)
  "Merge ENTRIES into HISTORY, preserving newest-first de-duplicated order."
  (let ((source-entries (if (command-history-p entries)
                            (command-history-entries entries)
                            entries)))
    (dolist (entry (reverse source-entries) history)
      (history-add history
                   (history-entry-text entry)
                   (history-entry-exit-code entry)))))
