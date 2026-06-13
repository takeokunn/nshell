(in-package #:nshell.presentation)
(defvar *running* nil)

(defun run-repl ()
  "CPS-based interactive REPL loop for nshell."
  (setf *running* t)
  (let* ((history (nshell.domain.history:make-command-history))
         (config (nshell.domain.configuration:default-config))
         (dispatcher (nshell.application:make-event-dispatcher))
         (kb (nshell.domain.completion:make-knowledge-base)))
    ;; Populate knowledge base with common commands
    (nshell.domain.completion:kb-add-command kb "ls" :flags '("-l" "-a" "-la" "-h"))
    (nshell.domain.completion:kb-add-command kb "cd")
    (nshell.domain.completion:kb-add-command kb "git" :subcommands '("status" "commit" "push" "pull") :flags '("-m"))
    (nshell.domain.completion:kb-add-command kb "echo")
    (nshell.domain.completion:kb-add-command kb "pwd")
    (nshell.domain.completion:kb-add-command kb "exit")
    (format t "nshell v0.1.0 - fish-inspired interactive shell~%")
    (format t "Type 'exit' to quit.~%~%")
    (loop while *running*
          do (render-prompt config nil)
             (finish-output)
             (let ((line (read-line *standard-input* nil nil)))
               (when (null line) (return))
               (when (string= line "exit") (return))
               (handler-case
                   (progn
                     (nshell.domain.history:history-add history line)
                     (let ((result (nshell.domain.parsing:parse-command-line line)))
                       (when (nshell.domain.parsing:parse-complete-p result)
                         (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                           (when (nshell.domain.parsing:command-node-p ast)
                             (let ((cmd (nshell.domain.parsing:command-node-command ast)))
                               ;; Execute basic built-in commands
                               (cond
                                 ((string= cmd "echo")
                                  (format t "~{~a~^ ~}~%"
                                          (nshell.domain.parsing:command-node-args ast)))
                                 ((string= cmd "pwd")
                                  (format t "~a~%" (uiop:getcwd)))
                                 ((string= cmd "ls")
                                  (let ((dir (uiop:getcwd)))
                                    (dolist (f (uiop:directory-files dir))
                                      (format t "~a~%" (file-namestring f)))))
                                 ((string= cmd "cd")
                                  (let ((args (nshell.domain.parsing:command-node-args ast)))
                                    (when args
                                      (handler-case (uiop:chdir (first args))
                                        (error (e) (format t "cd: ~a~%" e))))))
                                 (t
                                  (format t "nshell: command not found: ~a~%" cmd))))))))
                 (error (e)
                   (format t "nshell error: ~a~%" e))))))
  (format t "Goodbye!~%"))
