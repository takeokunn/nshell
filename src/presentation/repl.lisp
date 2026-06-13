(in-package #:nshell.presentation)

(defvar *running* nil)

(defun execute-builtin (ast history)
  "Execute a built-in command from AST. Returns T if handled, NIL if not a builtin."
  (let ((cmd (nshell.domain.parsing:command-node-command ast))
        (args (nshell.domain.parsing:command-node-args ast)))
    (cond
      ((string= cmd "echo")
       (format t "~{~a~^ ~}~%" args)
       t)
      ((string= cmd "pwd")
       (format t "~a~%" (uiop:getcwd))
       t)
      ((string= cmd "ls")
       (let ((dir (uiop:getcwd)))
         (handler-case
             (dolist (f (uiop:directory-files dir))
               (format t "~a~%" (file-namestring f)))
           (error (err)
             (format t "ls: ~a~%" err))))
       t)
      ((string= cmd "cd")
       (when args
         (handler-case (uiop:chdir (first args))
           (error (err) (format t "cd: ~a~%" err))))
       t)
      ((string= cmd "exit")
       (setf *running* nil)
       t)
      (t nil))))

(defun execute-external (ast)
  "Execute an external command via sb-ext:run-program."
  (let* ((cmd (nshell.domain.parsing:command-node-command ast))
         (args (nshell.domain.parsing:command-node-args ast))
         (all-args (cons cmd args)))
    (handler-case
        (let ((proc (sb-ext:run-program (first all-args) (rest all-args)
                                        :output :stream :error :stream :wait t)))
          (when proc
            (sb-ext:process-wait proc)))
      (error (err)
        (format t "nshell: ~a: ~a~%" cmd err)))))

(defun run-repl ()
  "Interactive REPL loop for nshell."
  (setf *running* t)
  (let* ((history (nshell.domain.history:make-command-history))
         (config (nshell.domain.configuration:default-config))
         (kb (nshell.domain.completion:make-knowledge-base)))
    ;; Populate knowledge base
    (nshell.domain.completion:kb-add-command kb "ls" :flags '("-l" "-a" "-la" "-h"))
    (nshell.domain.completion:kb-add-command kb "cd")
    (nshell.domain.completion:kb-add-command kb "echo")
    (nshell.domain.completion:kb-add-command kb "pwd")
    (nshell.domain.completion:kb-add-command kb "exit")
    (format t "nshell v0.1.0 - fish-inspired interactive shell~%")
    (format t "Type 'exit' to quit.~%~%")
    (loop while *running* do
      (render-prompt config nil)
      (finish-output)
      (let ((line (read-line *standard-input* nil nil)))
        (when (null line)
          (setf *running* nil))
        (when (and line (not (string= line "")))
          (handler-case
              (progn
                (nshell.domain.history:history-add history line)
                (let ((result (nshell.domain.parsing:parse-command-line line)))
                  (when (nshell.domain.parsing:parse-complete-p result)
                    (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                      (when (nshell.domain.parsing:command-node-p ast)
                        (or (execute-builtin ast history)
                            (execute-external ast)))))))
            (error (err)
              (format t "nshell error: ~a~%" err)))))))
  (format t "Goodbye!~%"))
