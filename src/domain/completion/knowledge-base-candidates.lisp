(in-package #:nshell.domain.completion)

(defun builtin-command-candidates (prefix)
  (sort (loop for entry in +builtin-command-catalog+
              for name = (getf entry :command)
              when (starts-with-p prefix name)
                collect (make-candidate name
                                        :kind :command
                                        :description (or (getf entry :description) "")))
        #'string<
        :key #'candidate-text))

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
      (sort (loop for name in (remove-duplicates
                               (append (copy-list (getf entry :flags))
                                       (copy-list (getf entry :subcommands)))
                               :test #'string=)
                  when (and (stringp name) (starts-with-p prefix name))
                    collect (make-candidate name :kind :option :description ""))
            #'string<
            :key #'candidate-text))))
