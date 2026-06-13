(in-package #:nshell.presentation)

(defvar *running* nil)

(defun trampoline (thunk)
  (loop for kont = (funcall thunk) then (funcall kont) while kont))
(defun done () nil)

;; ── Redirect extraction ────────────────────────────────────
(defun extract-redirects (args)
  "Separate redirect tokens from args. Returns (values clean-args redirects).
redirects is a list of (op . target) pairs where op is :> :>> or :<."
  (let ((clean nil)
        (redirects nil)
        (i 0))
    (loop while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((string= arg ">")
                (when (< (1+ i) (length args))
                  (push (cons :> (nth (1+ i) args)) redirects)
                  (incf i 2)))
               ((string= arg ">>")
                (when (< (1+ i) (length args))
                  (push (cons :>> (nth (1+ i) args)) redirects)
                  (incf i 2)))
               ((string= arg "<")
                (when (< (1+ i) (length args))
                  (push (cons :< (nth (1+ i) args)) redirects)
                  (incf i 2)))
               (t
                (push arg clean)
                (incf i))))
    (values (nreverse clean) (nreverse redirects))))

;; ── Command execution ──────────────────────────────────────
(defun execute-builtin (ast)
  (let ((cmd (nshell.domain.parsing:command-node-command ast))
        (args (nshell.domain.parsing:command-node-args ast)))
    (multiple-value-bind (clean-args redirects) (extract-redirects args)
      (apply-redirects redirects)
      (cond
        ((string= cmd "echo") (format t "~{~a~^ ~}~%" clean-args) t)
        ((string= cmd "pwd") (format t "~a~%" (uiop:getcwd)) t)
        ((string= cmd "ls")
         (handler-case (dolist (f (uiop:directory-files (uiop:getcwd)))
                         (format t "~a~%" (file-namestring f)))
           (error (err) (format t "ls: ~a~%" err))) t)
        ((string= cmd "cd")
         (when clean-args (handler-case (uiop:chdir (first clean-args))
                      (error (err) (format t "cd: ~a~%" err)))) t)
        ((string= cmd "exit") (setf *running* nil) t)
        (t nil)))))

(defun execute-ast (ast)
  (unwind-protect
       (cond
         ((nshell.domain.parsing:pipeline-node-p ast)
          (nshell.infrastructure.acl:spawn-pipeline
           (nshell.domain.parsing:pipeline-node-commands ast)))
         ((nshell.domain.parsing:command-node-p ast)
          (or (execute-builtin ast)
              (let* ((cmd (nshell.domain.parsing:command-node-command ast))
                     (args (nshell.domain.parsing:command-node-args ast)))
                (multiple-value-bind (clean-args redirects) (extract-redirects args)
                  (apply-redirects redirects)
                  (nshell.infrastructure.acl:run-external cmd clean-args)))))
         (t (format t "nshell: cannot execute~%")))
    (nshell.infrastructure.acl:restore-redirects)))

(defun apply-redirects (redirects)
  "Apply shell redirects to standard streams."
  (dolist (r redirects)
    (let ((op (car r)) (target (cdr r)))
      (case op
        (:> (nshell.infrastructure.acl:redirect-output target :supersede))
        (:>> (nshell.infrastructure.acl:redirect-output target :append))
        (:< (nshell.infrastructure.acl:redirect-input target))))))

;; ── Fish-style interactive input loop ──────────────────────
(defun read-char-raw ()
  (read-char *standard-input* nil nil))

(defstruct fish-input-state
  (buffer "" :type string)
  (completion-index -1 :type integer)
  (last-candidates nil :type list))

(defun fish-input-loop (history kb config)
  "Fish-style interactive input loop with non-destructive rendering."
  (let ((state (make-fish-input-state)))
    (loop
      (nshell.infrastructure.terminal:ansi-clear-line)
      (format t "~c" #\Return)
      (render-prompt config nil)
      (let ((text (fish-input-state-buffer state)))
        ;; Highlight
        (handler-case
            (let ((spans (highlight-line text)))
              (format t "~a" (highlight->ansi spans text
                                              (nshell.domain.configuration:config-theme config))))
          (error ()
            (format t "~a" text)))
        ;; Autosuggest
        (let ((suggestion (compute-suggestion history text)))
          (when (and suggestion (> (length suggestion) 0))
            (format t "~C[2m~a~C[0m" #\Esc suggestion #\Esc))))
      (finish-output)
      (let ((ch (read-char-raw)))
        (cond
          ((null ch) (setf *running* nil) (return))
          ((or (char= ch #\Newline) (char= ch #\Return))
           (let ((text (fish-input-state-buffer state)))
             (format t "~%")
             (when (not (string= text ""))
               (handler-case
                   (let ((result (nshell.domain.parsing:parse-command-line text)))
                     (when (nshell.domain.parsing:parse-complete-p result)
                       (nshell.domain.history:history-add history text)
                       ;; Persist to file history
                       (nshell.infrastructure.persistence:append-history-entry text)
                        (execute-ast (nshell.domain.parsing:parse-result-ast result))
                        ;; Restore redirects after execution
                        (nshell.infrastructure.acl:restore-redirects))))
                 (error (err)
                   (format t "nshell error: ~a~%" err)))))
           (return))
          ((char= ch #\Tab)
           (let ((text (fish-input-state-buffer state)))
             (when (> (length text) 0)
               (let* ((candidates (nshell.domain.completion:complete kb text))
                      (old-cands (fish-input-state-last-candidates state)))
                 ;; Reset index if candidates changed
                 (unless (equal candidates old-cands)
                   (setf (fish-input-state-completion-index state) -1))
                 (if candidates
                     (progn
                       ;; Cycle to next candidate
                       (let* ((n (length candidates))
                              (idx (mod (1+ (fish-input-state-completion-index state)) n)))
                         (setf (fish-input-state-completion-index state) idx)
                         (setf (fish-input-state-buffer state)
                               (nshell.domain.completion:candidate-text (nth idx candidates))))
                       (setf (fish-input-state-last-candidates state) candidates)
                       ;; Show candidates list
                       (render-completions candidates)
                       (nshell.infrastructure.terminal:ansi-clear-line)
                       (format t "~c" #\Return)
                       (render-prompt config nil)
                       (format t "~a" (fish-input-state-buffer state)))
                     ;; No candidates - reset
                     (setf (fish-input-state-completion-index state) -1
                           (fish-input-state-last-candidates state) nil))))))
          ((char= ch #\Backspace)
           (let ((text (fish-input-state-buffer state)))
             (when (> (length text) 0)
               (setf (fish-input-state-buffer state)
                     (subseq text 0 (1- (length text))))
               (setf (fish-input-state-completion-index state) -1))))
          ((char= (code-char 27) ch) (read-char-raw) (read-char-raw))
          ((char= (code-char 12) ch)
           (nshell.infrastructure.terminal:ansi-clear-screen))
          ((char= (code-char 3) ch)
           (format t "^C~%")
           (setf (fish-input-state-buffer state) ""
                 (fish-input-state-completion-index state) -1))
          ((char= (code-char 4) ch)
           (let ((text (fish-input-state-buffer state)))
             (when (string= text "")
               (setf *running* nil) (return))))
          ((and (>= (char-code ch) 32) (<= (char-code ch) 126))
           (setf (fish-input-state-buffer state)
                 (concatenate 'string (fish-input-state-buffer state) (string ch)))
           (setf (fish-input-state-completion-index state) -1))
          (t nil))))))

(defun run-repl ()
  "Fish-inspired interactive REPL with persistence and completion."
  (setf *running* t)
  (let* ((history (nshell.domain.history:make-command-history))
         (config (nshell.domain.configuration:default-config))
         (kb (nshell.domain.completion:make-knowledge-base)))
    ;; Load history from file
    (let ((saved (nshell.infrastructure.persistence:load-history-file)))
      (dolist (entry (reverse saved))
        (nshell.domain.history:history-add history entry)))
    (nshell.domain.completion:kb-add-command kb "ls" :flags '("-l" "-a"))
    (nshell.domain.completion:kb-add-command kb "cd")
    (nshell.domain.completion:kb-add-command kb "echo")
    (nshell.domain.completion:kb-add-command kb "pwd")
    (nshell.domain.completion:kb-add-command kb "exit")
    (handler-case (nshell.infrastructure.terminal:enable-raw-mode) (error ()))
    (unwind-protect
         (loop while *running* do (fish-input-loop history kb config))
      (nshell.infrastructure.terminal:restore-terminal-mode)
      (format t "Goodbye!~%"))))
