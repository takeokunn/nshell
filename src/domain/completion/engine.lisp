(in-package #:nshell.domain.completion)
(defun command-token (partial-input)
  (let ((space-position (position #\Space partial-input)))
    (if space-position (subseq partial-input 0 space-position) partial-input)))

(defun argument-prefix (partial-input)
  (let ((space-position (position #\Space partial-input :from-end t)))
    (if space-position (subseq partial-input (1+ space-position)) "")))

(defun starts-with-p (prefix text)
  (and (>= (length text) (length prefix))
       (string-equal prefix text :end2 (length prefix))))

(defun solution-value (variable solution)
  (cdr (assoc variable solution)))

(defun candidates-from-rule-solutions (solutions variable kind &key (prefix ""))
  (sort (loop for solution in solutions
              for value = (solution-value variable solution)
              when (and (stringp value) (starts-with-p prefix value))
                collect (make-candidate value :kind kind))
        #'string<
        :key #'candidate-text))

(defun rule-complete (kb-rules partial-input)
  (let* ((command (command-token partial-input))
         (arg-prefix (argument-prefix partial-input)))
    (cond
      ((and (< (length command) (length partial-input))
            (string= command "cd")
            (prove-all kb-rules (list 'suggests-dir command)))
       (list (make-candidate "" :kind :directory)))
      ((and (< (length command) (length partial-input))
            (string= command "source")
            (prove-all kb-rules (list 'suggests-file command)))
       (list (make-candidate "" :kind :file)))
      ((< (length command) (length partial-input))
       (candidates-from-rule-solutions
        (prove-all kb-rules (list 'completes command '?completion))
        '?completion
        :option
        :prefix arg-prefix))
      (t
       (candidates-from-rule-solutions
        (prove-all kb-rules '(completes ?command ?completion))
        '?command
        :command
        :prefix command)))))

(defun complete (kb partial-input)
  (or (rule-complete *built-in-rule-knowledge-base* partial-input)
      (let ((results '()))
        (maphash (lambda (name entry)
                   (declare (ignore entry))
                   (when (starts-with-p partial-input name)
                     (push (make-candidate name :kind :command) results)))
                 (knowledge-base-commands kb))
        (sort results #'string< :key #'candidate-text))))
(defun complete-command (kb prefix) (complete kb prefix))
(defun complete-argument (kb cmd-name arg-prefix)
  (or (mapcar #'candidate-text
              (candidates-from-rule-solutions
               (prove-all *built-in-rule-knowledge-base* (list 'completes cmd-name '?completion))
               '?completion
               :option
               :prefix arg-prefix))
      (let ((entry (kb-query kb cmd-name)))
        (when entry
          (let ((flags (getf entry :flags)))
            (when flags
              (remove-if-not (lambda (f) (starts-with-p arg-prefix f))
                             flags)))))))
