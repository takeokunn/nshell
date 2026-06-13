(in-package #:nshell.domain.history)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(command-history
            command-history-p
            make-command-history
            command-history-entries
            command-history-max-entries
            history-empty-p
            history-size)))

;;; CommandHistory entity - in-memory collection of history entries
;;; fish-inspired append-only model (in-memory for now, file-backed later)

(defstruct (command-history (:constructor make-command-history (&key (max-entries 10000))))
  "An append-only command history with search capabilities.
MAX-ENTRIES is the maximum number of entries to keep."
  (entries nil :type list)
  (max-entries 10000 :type integer :read-only t))

(defun make-history (&key (max-entries 10000))
  "Create an in-memory command history.
Compatibility wrapper for MAKE-COMMAND-HISTORY."
  (make-command-history :max-entries max-entries))

(defun history-add (history text &optional exit-code)
  "Add a new command to history. Returns the updated history.
Commands are deduplicated: an existing entry with the same text
is moved to the front (most recent) rather than duplicated."
  (let* ((entry (make-history-entry text (get-universal-time) exit-code))
         (existing (remove-if (lambda (e) (entry-equal-p e entry))
                              (command-history-entries history)))
         (new-entries (cons entry existing)))
    ;; Trim to max-entries.
    (setf (command-history-entries history)
          (subseq new-entries 0 (min (length new-entries)
                                     (command-history-max-entries history))))
    history))

(defun history-all (history)
  "Return all history entries, most recent first."
  (command-history-entries history))

(defun history-empty-p (history)
  "True if history has no entries."
  (null (command-history-entries history)))

;;; Search modes (fish-inspired)
(defun history-match-prefix (entry query &key case-sensitive)
  "True if entry text starts with query."
  (let ((et (history-entry-text entry))
        (q query))
    (if case-sensitive
        (and (>= (length et) (length q))
             (string= et q :end1 (length q)))
        (and (>= (length et) (length q))
             (string-equal et q :end1 (length q))))))

(defun history-match-exact (entry query &key case-sensitive)
  "True if entry text exactly matches query."
  (if case-sensitive
      (string= (history-entry-text entry) query)
      (string-equal (history-entry-text entry) query)))

(defun history-match-contains (entry query &key case-sensitive)
  "True if entry text contains query substring."
  (let ((et (history-entry-text entry)))
    (if case-sensitive
        (search query et)
        (search (string-downcase query) (string-downcase et)))))

(defun history-match-line-prefix (entry query &key case-sensitive)
  "True if entry text from cursor position has query as prefix.
For string queries, same as prefix for now."
  (history-match-prefix entry query :case-sensitive case-sensitive))

(defun history-search (history query &key (mode :prefix) (case-sensitive nil) (smartcase t))
  "Search history for entries matching QUERY.
MODE: :prefix, :exact, :contains, :line-prefix
CASE-SENSITIVE: force case-sensitive when SMARTCASE is NIL.
SMARTCASE: if query has uppercase, search case-sensitive.
Returns list of matching entries, most recent first."
  (let ((match-fn (ecase mode
                    (:prefix #'history-match-prefix)
                    (:exact #'history-match-exact)
                    (:contains #'history-match-contains)
                    (:line-prefix #'history-match-line-prefix)))
        (cs (if smartcase
                (some #'upper-case-p query)
                case-sensitive)))
    (remove-if-not (lambda (entry)
                     (funcall match-fn entry query :case-sensitive cs))
                   (command-history-entries history))))

;;; Utility
(defun history-size (history)
  "Return current number of entries in history."
  (length (command-history-entries history)))

(defun history-dedup (history)
  "Remove duplicate entries from history, keeping most recent.
Returns modified history."
  (let ((seen (make-hash-table :test #'equal)))
    (setf (command-history-entries history)
          (remove-if-not (lambda (e)
                           (let ((text (history-entry-text e)))
                             (unless (gethash text seen)
                               (setf (gethash text seen) t))))
                         (command-history-entries history)))
    history))

(defun history-merge (history entries)
  "Merge ENTRIES into HISTORY, preserving newest-first de-duplicated order.
ENTRIES may be another command-history or a list of history-entry values."
  (let ((source-entries (if (command-history-p entries)
                            (command-history-entries entries)
                            entries)))
    (dolist (entry (reverse source-entries) history)
      (history-add history (history-entry-text entry) (history-entry-exit-code entry)))))
