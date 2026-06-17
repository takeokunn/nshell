(in-package #:nshell.application)

(defparameter +complete-option-specs+
  '(("-c" :kind :command :requirement "command")
    ("--command" :kind :command :requirement "command")
    ("-f" :kind :flag :requirement "flag")
    ("--flag" :kind :flag :requirement "flag")
    ("-d" :kind :description :requirement "description")
    ("--description" :kind :description :requirement "description")))

(defmacro %with-complete-argument ((return-target remaining option requirement) &body body)
  `(if (rest ,remaining)
       (progn ,@body)
       (return-from ,return-target
         (values nil nil nil
                 (%required-argument-error "complete" ,option ,requirement)))))

(defun %complete-option-spec (option)
  (cdr (assoc option +complete-option-specs+ :test #'string=)))

(defun %complete-apply-option (spec remaining command flags description)
  (ecase (getf spec :kind)
    (:command
     (values (second remaining) flags description (cddr remaining)))
    (:flag
     (values command (cons (second remaining) flags) description (cddr remaining)))
    (:description
     (values command flags (second remaining) (cddr remaining)))))

(defun %parse-complete-args (args)
  (let ((command nil)
        (flags nil)
        (description nil)
        (remaining args))
    (%with-option-arguments (remaining option)
        (return)
      (return-from %parse-complete-args
          (values nil nil nil (format nil "complete: unknown option ~a" option)))
      (return)
      ((%complete-option-spec option)
       (let ((spec (%complete-option-spec option)))
         (%with-complete-argument (%parse-complete-args remaining option
                                                         (getf spec :requirement))
           (multiple-value-bind (new-command new-flags new-description new-remaining)
               (%complete-apply-option spec remaining command flags description)
             (setf command new-command
                   flags new-flags
                   description new-description
                   remaining new-remaining))))))
    (values command (nreverse flags) description nil)))

(defun %builtin-complete (context args)
  (multiple-value-bind (command flags description error)
      (%parse-complete-args args)
    (cond
      (error
       (values (format nil "~a~%" error) 2))
      ((null command)
       (%builtin-usage "complete" "complete -c command [-f flag ...] [-d description]"))
      (t
       (let* ((kb (shell-context-knowledge-base context))
              (entry (and kb (nshell.domain.completion:kb-query kb command)))
              (merged-flags (remove-duplicates
                             (append (or flags nil) (getf entry :flags))
                             :test #'string=)))
         (nshell.domain.completion:kb-add-command
          kb command
          :flags merged-flags
          :description (or description (getf entry :description)))
         (values nil 0))))))
