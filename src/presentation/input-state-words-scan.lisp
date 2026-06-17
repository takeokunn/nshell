;;; Shell token scanning helpers for the input reducer.

(in-package #:nshell.presentation)

(defun shell-token-end (text start)
  "Return the end index of the shell token in TEXT starting at START."
  (let ((pos start)
        (end (length text))
        (quote nil)
        (escaped nil))
    (block scan
      (loop while (< pos end)
            for ch = (char text pos)
            do (cond
                 ((eq quote #\')
                  (incf pos)
                  (when (char= ch #\')
                    (setf quote nil)))
                 (escaped
                  (setf escaped nil)
                  (incf pos))
                 ((char= ch #\\)
                  (setf escaped t)
                  (incf pos))
                 ((eq quote #\")
                  (incf pos)
                  (when (char= ch #\")
                    (setf quote nil)))
                 ((or (char= ch #\')
                      (char= ch #\"))
                  (setf quote ch)
                  (incf pos))
                 ((nshell.domain.parsing:shell-token-separator-p ch)
                  (return-from scan pos))
                 (t
                  (incf pos)))))
    pos))

(defun shell-token-ranges-before (text limit)
  "Return shell token ranges whose starts are before LIMIT."
  (let ((pos 0)
        (end (min limit (length text)))
        (ranges nil))
    (loop while (< pos end)
          do (if (nshell.domain.parsing:shell-token-separator-p
                  (char text pos))
                 (incf pos)
                 (let ((token-start pos)
                       (token-end (shell-token-end text pos)))
                   (push (cons token-start token-end) ranges)
                   (setf pos (max token-end (1+ pos))))))
    (nreverse ranges)))

(defun shell-token-range-at-position (text position)
  "Return the shell token range containing POSITION."
  (loop for range in (shell-token-ranges-before text (length text))
        when (and (<= (car range) position)
                  (< position (cdr range)))
          do (return (values (car range) (cdr range) t))
        finally (return (values nil nil nil))))

(defun shell-token-range-at-or-after-cursor (buffer cursor)
  (let* ((end (length buffer))
         (position (min cursor end))
         (ranges (shell-token-ranges-before buffer end)))
    (cond
      ((null ranges)
       (values nil nil nil))
      ((>= position end)
       (let ((range (car (last ranges))))
         (values (car range) (cdr range) t)))
      (t
       (loop for range in ranges
             when (or (and (<= (car range) position)
                           (< position (cdr range)))
                      (>= (car range) position))
               do (return (values (car range) (cdr range) t))
             finally (return (values nil nil nil)))))))

(defun shell-token-range-before-position (buffer position)
  (let ((previous nil))
    (dolist (range (shell-token-ranges-before buffer (length buffer)))
      (if (<= (cdr range) position)
          (setf previous range)
        (return)))
    (if previous
        (values (car previous) (cdr previous) t)
        (values nil nil nil))))

(defun previous-kill-word-start (buffer cursor)
  (let ((pos cursor))
    (loop while (and (> pos 0)
                     (nshell.domain.parsing:shell-token-separator-p
                      (char buffer (1- pos))))
          do (decf pos))
    (multiple-value-bind (start end found-p)
        (if (> pos 0)
            (shell-token-range-at-position buffer (1- pos))
            (values nil nil nil))
      (declare (ignore end))
      (if found-p
          start
          pos))))

(defun next-kill-word-end (buffer cursor)
  (let ((pos cursor)
        (end (length buffer)))
    (loop while (and (< pos end)
                     (nshell.domain.parsing:shell-token-separator-p
                      (char buffer pos)))
          do (incf pos))
    (if (< pos end)
        (multiple-value-bind (start token-end found-p)
            (shell-token-range-at-position buffer pos)
          (declare (ignore start))
          (if found-p
              token-end
              (shell-token-end buffer pos)))
        pos)))
