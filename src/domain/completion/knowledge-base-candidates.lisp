(in-package #:nshell.domain.completion)

(defun knowledge-base-command-candidates (kb prefix)
  (let ((results '()))
    (maphash (lambda (name entry)
               (when (starts-with-p prefix name)
                 (push (make-candidate name
                                       :kind :command
                                       :description (or (getf entry :description) ""))
                       results)))
             (knowledge-base-commands kb))
    results))

(defun knowledge-base-argument-candidates (kb command prefix)
  (let ((entry (kb-query kb command)))
    (when entry
      (sort (loop for flag in (getf entry :flags)
                  when (and (stringp flag) (starts-with-p prefix flag))
                    collect (make-candidate flag :kind :option :description ""))
            #'string<
            :key #'candidate-text))))
