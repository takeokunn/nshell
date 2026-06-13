;;; nshell REPL - CPS-based interactive shell loop
;;; fish-inspired UX with trampoline-driven continuations
(in-package #:nshell.presentation)

;; Trampoline
(defun trampoline (thunk)
  (loop for kont = (funcall thunk) then (funcall kont) while kont))
(defun done () nil)

;; REPL State
(defvar *running* nil)
(defvar *last-exit-code* 0)
(defvar *history* nil)
(defvar *config* nil)
(defvar *kb* nil)
(defvar *input-state* nil)
(defvar *environment* nil)
(defvar *aliases* (make-hash-table :test #'equal))
(defvar *abbreviations* (make-hash-table :test #'equal))

;; Redirect helpers
(defun extract-redirects (args)
  (let ((clean nil) (redirects nil) (i 0))
    (loop while (< i (length args))
          for val = (nth i args)
          do (cond
               ((string= val ">") (when (< (1+ i) (length args)) (push (cons :> (nth (1+ i) args)) redirects) (incf i 2)))
               ((string= val ">>") (when (< (1+ i) (length args)) (push (cons :>> (nth (1+ i) args)) redirects) (incf i 2)))
               ((string= val "<") (when (< (1+ i) (length args)) (push (cons :< (nth (1+ i) args)) redirects) (incf i 2)))
               (t (push val clean) (incf i))))
    (values (nreverse clean) (nreverse redirects))))

(defun apply-redirects (redirects)
  (dolist (r redirects)
    (let ((op (car r)) (target (cdr r)))
      (case op
        (:> (nshell.infrastructure.acl:redirect-output target :supersede))
        (:>> (nshell.infrastructure.acl:redirect-output target :append))
        (:< (nshell.infrastructure.acl:redirect-input target))))))

;; Builtins
(defun execute-builtin (ast)
  (let ((cmd (nshell.domain.parsing:command-node-command ast))
        (args (nshell.domain.parsing:command-node-arg-values ast)))
    (cond
      ((string= cmd "echo") (format t "~{~a~^ ~}~%" args) (values t 0))
      ((string= cmd "pwd") (format t "~a~%" (namestring (uiop:getcwd))) (values t 0))
      ((string= cmd "ls") (handler-case (dolist (f (uiop:directory-files (uiop:getcwd))) (format t "~a~%" (file-namestring f))) (error (e) (format t "ls: ~a~%" e))) (values t 0))
      ((string= cmd "cd") (if args (handler-case (uiop:chdir (first args)) (error (e) (format t "cd: ~a~%" e) (values t 1))) (values t 0)))
      ((string= cmd "exit") (setf *running* nil) (values t 0))
      ((string= cmd "fg") (nshell.application:fg (if args (parse-integer (first args) :junk-allowed t) 0)) (values t 0))
      ((string= cmd "bg") (nshell.application:bg (if args (parse-integer (first args) :junk-allowed t) 0)) (values t 0))
      ((string= cmd "jobs") (nshell.application:jobs) (values t 0))
      ((string= cmd "set")
       (cond
         ((and (>= (length args) 1) (string= (first args) "-x") (>= (length args) 3))
          (setf *environment* (nshell.domain.environment:env-set *environment* (second args) (third args) t))
          (values t 0))
         ((and (>= (length args) 1) (string= (first args) "-e") (>= (length args) 2))
          (setf *environment* (nshell.domain.environment:env-unset *environment* (second args)))
          (values t 0))
         ((>= (length args) 2)
          (setf *environment* (nshell.domain.environment:env-set *environment* (first args) (second args) nil))
          (values t 0))
         (t (format t "set: usage: set [-x] name value, or set -e name~%") (values t 1))))
      ((string= cmd "export")
       (if args
           (progn (setf *environment* (nshell.domain.environment:env-export *environment* (first args)))
                  (values t 0))
           (progn (format t "export: usage: export name~%") (values t 1))))
      ((string= cmd "alias")
       (if (>= (length args) 2)
           (progn (setf (gethash (first args) *aliases*) (second args))
                  (values t 0))
           (progn (maphash (lambda (k v) (format t "alias ~a=~a~%" k v)) *aliases*)
                  (values t 0))))
      ((string= cmd "abbr")
       (if (and (>= (length args) 2) (string= (first args) "-a") (>= (length args) 3))
           (progn (setf (gethash (second args) *abbreviations*) (third args))
                  (values t 0))
           (progn (format t "abbr: usage: abbr -a name expansion~%") (values t 1))))
      (t (values nil nil)))))

;; Execution
(defun execute-command-node (ast)
  (let* ((expanded-ast (apply-command-alias ast))
         (cmd (nshell.domain.parsing:command-node-command expanded-ast))
         (args (nshell.domain.parsing:command-node-args expanded-ast))
         (redirects nil))
    (when args
      (multiple-value-bind (clean r) (extract-redirects (expand-arg-list args))
        (setf args clean redirects r)))
    (unwind-protect
         (progn (apply-redirects redirects)
                (multiple-value-bind (builtin-p code) (execute-builtin (nshell.domain.parsing:make-command-node cmd args))
                  (if builtin-p code (nshell.infrastructure.acl:run-external cmd args))))
      (nshell.infrastructure.acl:restore-redirects))))

(defun execute-ast (ast)
  (cond
    ((nshell.domain.parsing:sequence-node-p ast)
     (let* ((cmds (nshell.domain.parsing:sequence-node-commands ast))
            (seps (nshell.domain.parsing:sequence-node-separators ast))
            (code 0))
       (loop for cmd in cmds for i from 0
             for sep = (and seps (< i (length seps)) (nth i seps))
             for prev-sep = (and (> i 0) (nth (1- i) seps))
             for bg-p = (eq :amp sep)
             ;; && and ||: check PREVIOUS separator for conditional execution
             for should-run = (or (= i 0)
                                  (not prev-sep)
                                  (eq :semi prev-sep)
                                  (eq :pipe prev-sep)
                                  (and (eq :and prev-sep) (= code 0))
                                  (and (eq :or prev-sep) (/= code 0)))
             do (when should-run
                  (if bg-p
                      (let ((proc (nshell.infrastructure.acl:spawn-async
                                   (nshell.domain.parsing:command-node-command cmd)
                                   (expand-arg-list (nshell.domain.parsing:command-node-args cmd)))))
                        (when proc
                          (let* ((job (make-job-from-ast cmd (nshell.domain.parsing:command-node-command cmd)))
                                 (jid (nshell.domain.job-control:monitor-add-job nshell.application:*job-monitor* job)))
                            (setf (nshell.domain.execution:job-pids job) (list (sb-ext:process-pid proc)))
                            (setf (nshell.domain.execution:job-background-p job) t)
                            (nshell.domain.job-control:monitor-update nshell.application:*job-monitor* jid :running)
                            (format t "[~d] ~d~%" jid (sb-ext:process-pid proc)))))
                      (setf code (or (execute-ast cmd) 0)))))
       code))
    ((nshell.domain.parsing:pipeline-node-p ast)
     ;; Route through application layer for CPS pipeline execution
     (nshell.application:execute-pipeline ast))
    ((nshell.domain.parsing:command-node-p ast) (execute-command-node ast))
    (t (format t "nshell: cannot execute~%") 1)))

(defun apply-command-alias (ast)
  (if (nshell.domain.parsing:command-node-p ast)
      (let* ((cmd (nshell.domain.parsing:command-node-command ast)) (alias (gethash cmd *aliases*)))
        (if alias (let ((r (nshell.domain.parsing:parse-command-line alias)))
                    (if (and (nshell.domain.parsing:parse-complete-p r) (nshell.domain.parsing:command-node-p (nshell.domain.parsing:parse-result-ast r)))
                        (nshell.domain.parsing:make-command-node (nshell.domain.parsing:command-node-command (nshell.domain.parsing:parse-result-ast r))
                                                                 (append (nshell.domain.parsing:command-node-args (nshell.domain.parsing:parse-result-ast r))
                                                                         (nshell.domain.parsing:command-node-args ast)))
                        ast)) ast)) ast))

(defun make-job-from-ast (ast text)
  (let* ((cmd (if (nshell.domain.parsing:command-node-p ast) ast
                  (first (nshell.domain.parsing:sequence-node-commands ast))))
         (dom-cmd (nshell.domain.execution:make-command (nshell.domain.parsing:command-node-command cmd)
                                                        (nshell.domain.parsing:command-node-arg-values cmd)))
         (pipe (nshell.domain.execution:make-pipeline dom-cmd))
         (job (nshell.domain.execution:make-job 0 pipe)))
    (setf (nshell.domain.execution:job-command-line job) text) job))

(defun expand-arg-list (args)
  (loop for arg in args
        for val = (if (consp arg) (car arg) arg)
        for qp = (and (consp arg) (cdr arg))
        if qp append (list val)
        else append (nshell.domain.expansion:expand-all val (ensure-environment))))

(defun ensure-environment ()
  (or *environment* (setf *environment* (nshell.domain.environment:make-default-environment))))

;; CPS Rendering
(defun render-prompt-cont ()
  (unless *running* (return-from render-prompt-cont (done)))
  (nshell.infrastructure.terminal:ansi-clear-line) (format t "~c" #\Return)
  (render-prompt *config* *last-exit-code*)
  (let* ((text (input-state-buffer *input-state*)) (theme (nshell.domain.configuration:config-theme *config*)))
    (handler-case (let ((spans (highlight-line text))) (format t "~a" (highlight->ansi spans text theme))) (error () (format t "~a" text)))
    (let ((sugg (input-state-suggestion *input-state*)))
      (when (and sugg (> (length sugg) 0)) (format t "~C[2m~a~C[0m" #\Esc sugg #\Esc))))
  (finish-output) (lambda () (read-key-cont)))

(defun read-key-cont ()
  (let ((event (nshell.infrastructure.terminal:read-key-event)))
    (if event (lambda () (process-key-cont event)) (progn (setf *running* nil) (done)))))

(defun process-key-cont (event)
  (multiple-value-bind (new-state output-event) (reduce-input-state *input-state* event)
    (setf *input-state* new-state)
    (case output-event
      (:execute
       (let ((text (input-state-buffer *input-state*)))
         (format t "~%")
         (unless (string= text "")
           (handler-case
               (let ((result (nshell.domain.parsing:parse-command-line text)))
                 (when (nshell.domain.parsing:parse-complete-p result)
                   (nshell.domain.history:history-add *history* text)
                   (nshell.domain.history:history-reset-navigation *history*)
                   (nshell.infrastructure.persistence:append-history-entry text)
                   ;; Sync exported environment to infrastructure as "KEY=VALUE" strings
                   (setf nshell.infrastructure.acl:*exported-environment*
                         (mapcar (lambda (pair)
                                   (format nil "~a=~a" (car pair) (cdr pair)))
                                 (nshell.domain.environment:env-list *environment*)))
                   (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                     (setf *last-exit-code* (or (execute-ast ast) 0)))))
             (error (e) (format t "nshell error: ~a~%" e) (setf *last-exit-code* 1))))
         (setf *input-state* (make-input-state))
         (lambda () (render-prompt-cont))))
      (:quit (setf *running* nil) (done))
      (:complete
       (let* ((text (input-state-buffer *input-state*))
              (cands (when (> (length text) 0) (nshell.domain.completion:complete *kb* text))))
         (when cands (setf (input-state-last-candidates *input-state*) (mapcar #'nshell.domain.completion:candidate-text cands)) (render-completions cands)))
       (lambda () (render-prompt-cont)))
      (:suggest-update
       (let ((text (input-state-buffer *input-state*)))
         (setf (input-state-suggestion *input-state*) (compute-suggestion *history* text)))
       (lambda () (render-prompt-cont)))
      (:history-prev
       (let ((entry (nshell.domain.history:history-previous *history* (input-state-buffer *input-state*))))
         (when entry (setf *input-state* (make-input-state :buffer entry :cursor-pos (length entry)))))
       (lambda () (render-prompt-cont)))
      (:history-next
       (let ((entry (nshell.domain.history:history-next *history*)))
         (when entry (setf *input-state* (make-input-state :buffer entry :cursor-pos (length entry)))))
       (lambda () (render-prompt-cont)))
      (:redraw (lambda () (render-prompt-cont)))
      (t (lambda () (render-prompt-cont))))))

;; REPL Entry
(defun run-repl ()
  (setf *running* t *last-exit-code* 0
        *history* (nshell.domain.history:make-command-history)
        *config* (nshell.domain.configuration:default-config)
        *kb* (nshell.domain.completion:make-knowledge-base)
        *input-state* (make-input-state)
        *environment* (nshell.domain.environment:make-default-environment))
  ;; Wire domain expansion to infrastructure (DDD purity)
  (setf nshell.domain.expansion:*glob-directory-files-fn*
        (lambda (dir) (uiop:directory-files dir)))
  (setf nshell.domain.expansion:*glob-subdirectories-fn*
        (lambda (dir) (uiop:subdirectories dir)))
  (let ((saved (nshell.infrastructure.persistence:load-history-file)))
    (dolist (e (reverse saved)) (nshell.domain.history:history-add *history* e)))
  (dolist (c '("ls" "cd" "echo" "pwd" "exit" "fg" "bg" "jobs" "set" "export" "alias" "abbr"))
    (nshell.domain.completion:kb-add-command *kb* c))
  (nshell.domain.completion:kb-add-command *kb* "ls" :flags '("-l" "-a" "-h" "-R"))
  (nshell.domain.completion:kb-add-command *kb* "set" :flags '("-x" "-e"))
  (nshell.domain.completion:kb-add-command *kb* "abbr" :flags '("-a"))
  (handler-case (nshell.infrastructure.terminal:enable-raw-mode) (error ()))
  (handler-case (nshell.infrastructure.acl:install-signal-handlers)
    (error (e) (format t "Warning: signal handlers: ~a~%" e)))
  (handler-case (progn (setf nshell.application:*shell-pgid* (sb-posix:getpid))
                       (nshell.infrastructure.acl:set-process-group 0 0)
                       (nshell.infrastructure.acl:set-foreground-pgroup nshell.application:*shell-pgid*))
    (error ()))
  (unwind-protect
       (trampoline (lambda () (render-prompt-cont)))
    (nshell.infrastructure.terminal:restore-terminal-mode)
    (format t "Goodbye!~%")))

;; ── Batch Mode ──────────────────────────────────────────
(defun run-repl-batch ()
  "Batch (non-interactive) mode: read lines, execute commands, print raw output."
  (setf *running* t *last-exit-code* 0
        *environment* (nshell.domain.environment:make-default-environment))
  (setf nshell.domain.expansion:*glob-directory-files-fn*
        (lambda (dir) (uiop:directory-files dir)))
  (setf nshell.domain.expansion:*glob-subdirectories-fn*
        (lambda (dir) (uiop:subdirectories dir)))
  (loop for line = (read-line *standard-input* nil nil)
        while (and line *running*)
        do (handler-case
               (let ((result (nshell.domain.parsing:parse-command-line line)))
                 (when (nshell.domain.parsing:parse-complete-p result)
                   (setf nshell.infrastructure.acl:*exported-environment*
                         (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair)))
                                 (nshell.domain.environment:env-list *environment*)))
                   (let ((ast (nshell.domain.parsing:parse-result-ast result)))
                     (setf *last-exit-code* (or (execute-ast ast) 0)))))
             (error (e)
               (format *error-output* "nshell error: ~a~%" e)
               (setf *last-exit-code* 1)))))
