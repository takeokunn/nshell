(in-package #:nshell.presentation)

(defvar *running* nil)

;; ── CPS Trampoline ──────────────────────────────────────────
(defun trampoline (thunk)
  "Run a CPS computation to completion."
  (loop for kont = (funcall thunk) then (funcall kont)
        while kont))

(defun done ()
  "Terminal continuation - stops the trampoline."
  nil)

;; ── Command Execution ───────────────────────────────────────
(defun execute-builtin (ast history)
  (let ((cmd (nshell.domain.parsing:command-node-command ast))
        (args (nshell.domain.parsing:command-node-args ast)))
    (cond
      ((string= cmd "echo")
       (format t "~{~a~^ ~}~%" args) t)
      ((string= cmd "pwd")
       (format t "~a~%" (uiop:getcwd)) t)
      ((string= cmd "ls")
       (handler-case
           (dolist (f (uiop:directory-files (uiop:getcwd)))
             (format t "~a~%" (file-namestring f)))
         (error (err) (format t "ls: ~a~%" err)))
       t)
      ((string= cmd "cd")
       (when args
         (handler-case (uiop:chdir (first args))
           (error (err) (format t "cd: ~a~%" err))))
       t)
      ((string= cmd "exit")
       (setf *running* nil) t)
      (t nil))))

(defun execute-ast (ast history)
  (cond
    ((nshell.domain.parsing:pipeline-node-p ast)
     (nshell.application:execute-pipeline ast))
    ((nshell.domain.parsing:command-node-p ast)
     (or (execute-builtin ast history)
         (let ((cmd (nshell.domain.parsing:command-node-command ast))
               (args (nshell.domain.parsing:command-node-args ast)))
           (nshell.application:execute-external cmd args))))
    (t (format t "nshell: cannot execute~%"))))

;; ── REPL with CPS Trampoline and Fish-style Features ────────
(defun run-repl ()
  "CPS-based interactive REPL with fish-inspired features."
  (setf *running* t)
  (let* ((history (nshell.domain.history:make-command-history))
         (config (nshell.domain.configuration:default-config))
         (kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "ls" :flags '("-l" "-a"))
    (nshell.domain.completion:kb-add-command kb "cd")
    (nshell.domain.completion:kb-add-command kb "echo")
    (nshell.domain.completion:kb-add-command kb "pwd")
    (nshell.domain.completion:kb-add-command kb "exit")
    (format t "nshell> Type 'exit' to quit.~%~%")
    ;; CPS trampoline-driven REPL
    (trampoline
     (lambda ()
       (render-prompt config nil)
       (finish-output)
       (let ((line (read-line *standard-input* nil nil)))
         (cond
           ((null line) (done))
           ((string= line "") (lambda () (run-repl-step history config kb)))
           ((string= line "exit") (progn (setf *running* nil) (done)))
           (t
            (handler-case
                (let ((result (nshell.domain.parsing:parse-command-line line)))
                  (when (nshell.domain.parsing:parse-complete-p result)
                    (nshell.domain.history:history-add history line)
                    (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                      ;; Autosuggest: show suggestion from history
                      (let ((suggestion (compute-suggestion history line)))
                        (declare (ignore suggestion)))
                      ;; Highlight: show syntax-colored input
                      (let* ((spans (highlight-line line))
                             (highlighted (highlight->ansi spans line
                                                          (nshell.domain.configuration:config-theme config))))
                        (declare (ignore highlighted)))
                      (execute-ast ast history))))
              (error (err)
                (format t "nshell error: ~a~%" err)))
            (if *running*
                (lambda () (run-repl-step history config kb))
                (done))))))))

(defun run-repl-step (history config kb)
  "Single step of the REPL - returns continuation."
  (declare (ignore kb))
  (render-prompt config nil)
  (finish-output)
  (let ((line (read-line *standard-input* nil nil)))
    (cond
      ((null line) (done))
      ((string= line "") (lambda () (run-repl-step history config kb)))
      ((string= line "exit") (progn (setf *running* nil) (done)))
      (t
       (handler-case
           (let ((result (nshell.domain.parsing:parse-command-line line)))
             (when (nshell.domain.parsing:parse-complete-p result)
               (nshell.domain.history:history-add history line)
               (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                 (compute-suggestion history line)
                 (execute-ast ast history))))
         (error (err)
           (format t "nshell error: ~a~%" err)))
       (if *running*
           (lambda () (run-repl-step history config kb))
           (done))))))

(defun run-repl-simple ()
  "Simple loop-based REPL (fallback)."
  (setf *running* t)
  (let* ((history (nshell.domain.history:make-command-history))
         (config (nshell.domain.configuration:default-config))
         (kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "ls" :flags '("-l" "-a"))
    (nshell.domain.completion:kb-add-command kb "cd")
    (nshell.domain.completion:kb-add-command kb "echo")
    (nshell.domain.completion:kb-add-command kb "pwd")
    (nshell.domain.completion:kb-add-command kb "exit")
    (loop while *running* do
      (render-prompt config nil)
      (finish-output)
      (let ((line (read-line *standard-input* nil nil)))
        (when (null line) (setf *running* nil))
        (when (and line (not (string= line "")))
          (handler-case
              (let ((result (nshell.domain.parsing:parse-command-line line)))
                (when (nshell.domain.parsing:parse-complete-p result)
                  (nshell.domain.history:history-add history line)
                  (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                    (execute-ast ast history))))
            (error (err)
              (format t "nshell error: ~a~%" err)))))))
  (format t "Goodbye!~%"))
