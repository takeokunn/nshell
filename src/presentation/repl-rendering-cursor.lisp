(in-package #:nshell.presentation)

(defun %wrap-rendered-position (row column terminal-width)
  (if (and terminal-width
           (plusp terminal-width)
           (> column terminal-width))
      (values (1+ row) (- column terminal-width))
      (values row column)))

(defun %advance-rendered-character (row column char terminal-width)
  (if (char= char #\Newline)
      (values (1+ row) 2)
      (let ((char-width (%char-visible-width char)))
        (when (and terminal-width
                   (plusp terminal-width)
                   (> (+ column char-width) terminal-width))
          (setf row (1+ row)
                column 0))
        (%wrap-rendered-position row (+ column char-width) terminal-width))))

(defun %advance-rendered-string (row column text terminal-width)
  (loop for char across (or text "")
        do (multiple-value-setq (row column)
             (%advance-rendered-character row column char terminal-width))
        finally (return (values row column))))

(defun %initial-rendered-position (prompt-width terminal-width)
  (if (and terminal-width
           (plusp terminal-width)
           (> prompt-width terminal-width))
      (values (floor (1- prompt-width) terminal-width)
              (1+ (mod (1- prompt-width) terminal-width)))
      (values 0 prompt-width)))

(defun %rendered-buffer-line-count (text &key suggestion search-suffix terminal-width
                                         (prompt-width 0))
  (multiple-value-bind (row column)
      (%initial-rendered-position prompt-width terminal-width)
    (multiple-value-setq (row column)
      (%advance-rendered-string row column text terminal-width))
    (multiple-value-setq (row column)
      (%advance-rendered-string row column suggestion terminal-width))
    (multiple-value-setq (row column)
      (%advance-rendered-string row column search-suffix terminal-width))
    (1+ row)))

(defun %cursor-tail-visible-width (text cursor suggestion search-suffix)
  (let* ((clamped-cursor (max 0 (min cursor (length text))))
         (tail (subseq text clamped-cursor)))
    (+ (%string-visible-width tail)
       (%string-visible-width (or suggestion ""))
       (%string-visible-width (or search-suffix "")))))

(defun %move-cursor-left-by-visible-width (columns)
  (when (plusp columns)
    (format t "~C[~dD" #\Esc columns)))

(defun %rendered-buffer-position (text cursor prompt-width &key terminal-width)
  (multiple-value-bind (initial-row initial-column)
      (%initial-rendered-position prompt-width terminal-width)
    (loop with row = initial-row
          with column = initial-column
          for index below (max 0 (min cursor (length text)))
          for char = (char text index)
          do (multiple-value-setq (row column)
               (%advance-rendered-character row column char terminal-width))
          finally (return (values row column)))))

(defun %move-cursor-to-rendered-position (text cursor prompt-width suggestion search-suffix
                                          &key terminal-width)
  (multiple-value-bind (target-row target-column)
      (%rendered-buffer-position text cursor prompt-width
                                 :terminal-width terminal-width)
    (multiple-value-bind (final-row final-column)
        (%rendered-buffer-position text (length text) prompt-width
                                   :terminal-width terminal-width)
      (multiple-value-setq (final-row final-column)
        (%advance-rendered-string final-row final-column suggestion terminal-width))
      (multiple-value-setq (final-row final-column)
        (%advance-rendered-string final-row final-column search-suffix terminal-width))
      (let ((rows-up (- final-row target-row)))
        (cond
          ((plusp rows-up)
           (format t "~C[~dA~C[~dG" #\Esc rows-up #\Esc (1+ target-column)))
          ((plusp (- final-column target-column))
           (%move-cursor-left-by-visible-width (- final-column target-column))))))))
