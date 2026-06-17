(in-package #:nshell.domain.completion)

(defun solution-value (variable solution)
  (cdr (assoc variable solution)))

(defun completion-description (kb-rules value)
  (when (stringp value)
    (let* ((solutions (prove-all kb-rules (list 'describes value '?description)))
           (description (and solutions
                             (solution-value '?description (first solutions)))))
      (when (stringp description)
        description))))

(defun candidates-from-rule-solutions (solutions variable kind &key (prefix "") description-fn)
  (sort (merge-candidates
         (loop for solution in solutions
               for value = (solution-value variable solution)
               for description = (and description-fn
                                      (funcall description-fn value))
               when (and (stringp value) (starts-with-p prefix value))
                 collect (make-candidate value
                                         :kind kind
                                         :description (or description ""))))
        #'string<
        :key #'candidate-text))

(defun rule-complete (kb-rules partial-input)
  (let* ((context (completion-context-for partial-input))
         (command (completion-context-command context))
         (arg-prefix (completion-context-argument-prefix context)))
    (flet ((candidate-description-for (value)
             (completion-description kb-rules value)))
      (cond
        ((completion-context-redirection-target-p context)
         (list (make-candidate arg-prefix :kind :file :description "file")))
        ((and (< (length command) (length partial-input))
              (prove-all kb-rules (list 'suggests-dir command)))
         (list (make-candidate "" :kind :directory :description "directory")))
        ((and (< (length command) (length partial-input))
              (prove-all kb-rules (list 'suggests-file command)))
         (list (make-candidate "" :kind :file :description "file")))
        ((< (length command) (length partial-input))
         (candidates-from-rule-solutions
          (prove-all kb-rules (list 'completes command '?completion))
          '?completion
          :option
          :prefix arg-prefix
          :description-fn #'candidate-description-for))
        (t
         (candidates-from-rule-solutions
          (prove-all kb-rules '(completes ?command ?completion))
          '?command
          :command
          :prefix command
          :description-fn #'candidate-description-for))))))

(defun complete (kb partial-input &key path)
  (let* ((context (completion-context-for partial-input))
         (rule-candidates (rule-complete *built-in-rule-knowledge-base* partial-input))
         (command (completion-context-command context))
         (arg-prefix (completion-context-argument-prefix context))
         (filesystem-mode (completion-filesystem-mode context))
         (filesystem-candidates
           (when filesystem-mode
             (filesystem-candidates-for-mode filesystem-mode arg-prefix))))
    (cond
      ((completion-context-command-position-p context)
       (rank-candidates
        command
        (merge-candidates
         rule-candidates
         (knowledge-base-command-candidates kb command)
         (command-candidates-from-path path command))))
      (filesystem-mode
       (rank-candidates
        arg-prefix
        (if filesystem-candidates
            filesystem-candidates
            rule-candidates)))
      (t
       (rank-candidates
        arg-prefix
        (merge-candidates
         rule-candidates
         (knowledge-base-argument-candidates kb command arg-prefix)))))))
