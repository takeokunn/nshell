(in-package #:nshell.presentation)

(defvar *running* nil)

(defun trampoline (thunk)
  (loop for kont = (funcall thunk) then (funcall kont) while kont))
(defun done () nil)

(defun execute-builtin (ast)
  (let ((cmd (nshell.domain.parsing:command-node-command ast))
        (args (nshell.domain.parsing:command-node-args ast)))
    (cond
      ((string= cmd "echo") (format t "~{~a~^ ~}~%" args) t)
      ((string= cmd "pwd") (format t "~a~%" (uiop:getcwd)) t)
      ((string= cmd "ls")
       (handler-case (dolist (f (uiop:directory-files (uiop:getcwd)))
                       (format t "~a~%" (file-namestring f)))
         (error (err) (format t "ls: ~a~%" err))) t)
      ((string= cmd "cd")
       (when args (handler-case (uiop:chdir (first args))
                    (error (err) (format t "cd: ~a~%" err)))) t)
      ((string= cmd "exit") (setf *running* nil) t)
      (t nil))))

(defun execute-ast (ast)
  (cond
    ((nshell.domain.parsing:pipeline-node-p ast)
     (nshell.infrastructure.acl:spawn-pipeline
      (nshell.domain.parsing:pipeline-node-commands ast)))
    ((nshell.domain.parsing:command-node-p ast)
     (or (execute-builtin ast)
         (let ((cmd (nshell.domain.parsing:command-node-command ast))
               (args (nshell.domain.parsing:command-node-args ast)))
           (nshell.infrastructure.acl:run-external cmd args))))
    (t (format t "nshell: cannot execute~%"))))

;; ── Fish-style interactive input loop with persistent string buffer ──
(defun read-char-raw ()
  "Read a single character from standard input, or NIL on EOF."
  (read-char *standard-input* nil nil))

(defstruct fish-input-state
  (buffer "" :type string)
  (pos 0 :type integer))

(defun fish-input-loop (history kb config)
  "Fish-style interactive input loop with non-destructive rendering."
  (let ((state (make-fish-input-state)))
    (loop
      ;; Render current state
      (nshell.infrastructure.terminal:ansi-clear-line)
      (format t "~c" #\Return)
      (render-prompt config nil)
      (let ((text (fish-input-state-buffer state)))
        ;; Highlight and display
        (handler-case
            (let ((spans (highlight-line text)))
              (format t "~a" (highlight->ansi spans text
                                              (nshell.domain.configuration:config-theme config))))
          (error ()
            (format t "~a" text)))
        ;; Autosuggestion
        (let ((suggestion (compute-suggestion history text)))
          (when (and suggestion (> (length suggestion) 0))
            (format t "~C[2m~a~C[0m" #\Esc suggestion #\Esc))))
      (finish-output)
      ;; Read next character
      (let ((ch (read-char-raw)))
        (cond
          ((null ch)
           (setf *running* nil)
           (return))
          ((or (char= ch #\Newline) (char= ch #\Return))
           (let ((text (fish-input-state-buffer state)))
             (format t "~%")
             (when (not (string= text ""))
               (handler-case
                   (let ((result (nshell.domain.parsing:parse-command-line text)))
                     (when (nshell.domain.parsing:parse-complete-p result)
                       (nshell.domain.history:history-add history text)
                       (execute-ast (nshell.domain.parsing:parse-result-ast result))))
                 (error (err)
                   (format t "nshell error: ~a~%" err)))))
           (return))
          ((char= ch #\Tab)
           (let ((text (fish-input-state-buffer state)))
             (when (> (length text) 0)
               (let ((candidates (nshell.domain.completion:complete kb text)))
                 (render-completions candidates)
                 ;; Restore display after completions
                 (format t "~c" #\Return)
                 (render-prompt config nil)
                 (format t "~a" text)))))
          ((char= ch #\Backspace)
           (let ((text (fish-input-state-buffer state)))
             (when (> (length text) 0)
               (setf (fish-input-state-buffer state)
                     (subseq text 0 (1- (length text)))))))
          ((char= (code-char 27) ch)
           ;; ESC - consume arrow key sequence
           (read-char-raw)
           (read-char-raw))
          ((char= (code-char 12) ch)
           ;; Ctrl-L - clear screen
           (nshell.infrastructure.terminal:ansi-clear-screen))
          ((char= (code-char 3) ch)
           (format t "^C~%")
           (setf (fish-input-state-buffer state) ""))
          ((char= (code-char 4) ch)
           (let ((text (fish-input-state-buffer state)))
             (when (string= text "")
               (setf *running* nil)
               (return))))
          ((and (>= (char-code ch) 32) (<= (char-code ch) 126))
           ;; Append printable character to buffer
           (setf (fish-input-state-buffer state)
                 (concatenate 'string (fish-input-state-buffer state) (string ch))))
          (t nil))))))

(defun run-repl ()
  "Fish-inspired interactive REPL with real-time features."
  (setf *running* t)
  (let* ((history (nshell.domain.history:make-command-history))
         (config (nshell.domain.configuration:default-config))
         (kb (nshell.domain.completion:make-knowledge-base)))
    (nshell.domain.completion:kb-add-command kb "ls" :flags '("-l" "-a"))
    (nshell.domain.completion:kb-add-command kb "cd")
    (nshell.domain.completion:kb-add-command kb "echo")
    (nshell.domain.completion:kb-add-command kb "pwd")
    (nshell.domain.completion:kb-add-command kb "exit")
    (handler-case (nshell.infrastructure.terminal:enable-raw-mode)
      (error ()))
    (unwind-protect
         (loop while *running* do
           (fish-input-loop history kb config))
      (nshell.infrastructure.terminal:restore-terminal-mode)
      (format t "Goodbye!~%"))))
