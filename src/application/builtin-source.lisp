(in-package #:nshell.application)

(defun %builtin-source (context args)
  (if args
      (handler-case
          (with-open-file (stream (first args) :direction :input)
            (%source-lines context (%collect-source-lines stream) (first args)))
        (error (condition)
          (values (format nil "source: ~a: ~a~%" (first args) condition) 1)))
      (%builtin-usage "source" "source file")))

(defun %execute-source-line (context line)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (if (string= trimmed "")
        (values nil 0)
        (labels ((parse-error-result (result)
                   (values (format nil "source: parse error: ~a~%"
                                   (nshell.domain.parsing:format-parse-error-messages result))
                           2)))
          (nshell.domain.parsing:with-parsed-command-line-case (result ast trimmed)
            (:complete
             (execute-ast-in-context context ast))
            (:error
             (parse-error-result result))
            (:incomplete
             (parse-error-result result)))))))

(defun %execute-external-pipeline-stage (command-node input redirects)
  (let* ((command (nshell.domain.parsing:command-node-command command-node))
         (args (%line-command-args command-node))
         (input-target (%input-redirect-target redirects))
         (opened-input nil)
         (stdin (cond
                  (input-target
                   (setf opened-input
                         (open input-target
                               :direction :input
                               :if-does-not-exist :error)))
                  (input (make-string-input-stream input))
                  (t *standard-input*))))
    (handler-case
        (unwind-protect
             (let ((process
                     (sb-ext:run-program command args
                                         :input stdin
                                         :output :stream
                                         :error :output
                                         :wait nil
                                         :search t)))
               (let ((output (%read-stream-to-string (sb-ext:process-output process))))
                 (sb-ext:process-wait process)
                 (values (and (not (%write-redirected-stage-output redirects output))
                              output)
                         (or (sb-ext:process-exit-code process) 0))))
          (when opened-input
            (close opened-input)))
      (error (condition)
        (values (format nil "nshell: ~a: ~a~%" command condition) 127)))))

(defun %execute-pipeline-stage-in-context (context command-node input redirects)
  (if (%shell-internal-command-p context command-node)
      (if input
          (with-input-from-string (*standard-input* input)
            (%execute-clean-command-node-in-context context command-node redirects))
          (%execute-clean-command-node-in-context context command-node redirects))
      (%execute-external-pipeline-stage command-node input redirects)))

(defun %execute-source-pipeline-in-context (context commands redirects)
  (let ((input nil)
        (code 0))
    (loop for command in commands
          for command-redirects in redirects
          do
      (multiple-value-bind (output exit-code)
          (%execute-pipeline-stage-in-context context command input command-redirects)
        (setf input output
              code (or exit-code 0))))
    (values input code)))

(defun execute-pipeline-node-in-context (context pipeline-node)
  (let ((commands (nshell.domain.parsing:pipeline-node-commands pipeline-node)))
    (multiple-value-bind (clean-commands redirects)
        (%extract-pipeline-redirects
         (mapcar (lambda (command)
                   (%expand-command-node-in-context context command))
                 commands))
      (if (or (eq :cps (shell-context-execution-strategy context))
              (some (lambda (command)
                      (%shell-internal-command-p context command))
                    clean-commands))
          (%execute-source-pipeline-in-context context clean-commands redirects)
          (let ((exit-code 0))
            (let ((output
                    (with-output-to-string (*standard-output*)
                      (setf exit-code
                            (or (nshell.infrastructure.acl:spawn-pipeline
                                 clean-commands
                                 :redirects redirects)
                                0)))))
              (values output exit-code)))))))

(defun %redirect-fn (context key)
  (getf (shell-context-redirect-fns context) key))

(defun %output-redirect-p (redirects)
  (find-if (lambda (redirect)
             (member (car redirect) '(:> :>>)))
           redirects))

(defun %apply-context-redirects (context redirects)
  (dolist (redirect redirects)
    (let ((target (cdr redirect)))
      (ecase (car redirect)
        (:> (funcall (%redirect-fn context :redirect-output) target :supersede))
        (:>> (funcall (%redirect-fn context :redirect-output) target :append))
        (:< (funcall (%redirect-fn context :redirect-input) target))))))

(defun %restore-context-redirects (context)
  (let ((restore (%redirect-fn context :restore)))
    (when restore
      (funcall restore))))

(defun %expand-command-node-in-context (context command-node)
  (let ((expanded-command (expand-command-alias-node
                           command-node
                           (shell-context-alias-table context))))
    (nshell.domain.parsing:make-command-node
     (nshell.domain.parsing:command-node-command expanded-command)
     (%line-command-args-in-context context expanded-command))))

(defun %execute-clean-command-node-in-context (context clean-command redirects)
  (let* ((command (nshell.domain.parsing:command-node-command clean-command))
         (args (%line-command-args clean-command))
         (redirect-output-p (%output-redirect-p redirects)))
    (unwind-protect
         (progn
           (when redirects
             (%apply-context-redirects context redirects))
           (multiple-value-bind (output code)
               (%execute-command-by-name-in-context context command args)
             (when (and redirect-output-p output)
               (write-string output))
             (values (and (not redirect-output-p) output) code)))
      (when redirects
        (%restore-context-redirects context)))))

(defun execute-command-node-in-context (context command-node)
  (multiple-value-bind (clean-command redirects)
      (%extract-command-redirects
       (%expand-command-node-in-context context command-node))
    (%execute-clean-command-node-in-context context clean-command redirects)))

(defun %shell-internal-command-p (context command-node)
  (let ((command (nshell.domain.parsing:command-node-command command-node)))
    (or (lookup-builtin command)
        (nth-value 1 (gethash command (shell-context-function-table context))))))

(defmacro %with-output-code-accumulator ((output code) &body body)
  `(let ((,output nil)
         (,code 0))
     ,@body
     (values (apply #'concatenate 'string (nreverse ,output)) ,code)))

(defmacro %collect-execution-result ((output code) form &optional (code-value 'exit-code))
  `(multiple-value-bind (chunk exit-code)
       ,form
     (when chunk
       (push chunk ,output))
     (setf ,code ,code-value)))

(defun %execute-condition-in-context (context condition)
  (if condition
      (execute-ast-in-context context condition)
      (values nil 1)))

(defun %execute-ast-list-in-context (context nodes)
  (%with-output-code-accumulator (output code)
    (dolist (node nodes)
      (%collect-execution-result
       (output code)
       (execute-ast-in-context context node)
       (or exit-code 0)))))

(defun %execute-if-node-in-context (context ast)
  (multiple-value-bind (condition-output condition-code)
      (%execute-condition-in-context
       context
       (nshell.domain.parsing:if-node-condition ast))
    (declare (ignore condition-output))
    (cond
      ((= 0 condition-code)
       (%execute-ast-list-in-context
        context
        (nshell.domain.parsing:if-node-then-branch ast)))
      ((nshell.domain.parsing:if-node-else-branch ast)
       (%execute-ast-list-in-context
        context
        (nshell.domain.parsing:if-node-else-branch ast)))
      (t (values nil 0)))))

(defun %execute-for-node-in-context (context ast)
  (%with-output-code-accumulator (output code)
    (dolist (value (loop for value-arg in (nshell.domain.parsing:for-node-in-values ast)
                         append (%expand-source-arg-in-context
                                 context
                                 value-arg)))
      (setf (shell-context-environment context)
            (nshell.domain.environment:env-set
             (shell-context-environment context)
             (nshell.domain.parsing:for-node-var-name ast)
             value
             nil))
      (%collect-execution-result
       (output code)
       (%execute-ast-list-in-context
        context
        (nshell.domain.parsing:for-node-body ast))))))

(defun %execute-while-node-in-context (context ast)
  (%with-output-code-accumulator (output code)
    (loop
      (multiple-value-bind (condition-output condition-code)
          (%execute-condition-in-context
           context
           (nshell.domain.parsing:while-node-condition ast))
        (declare (ignore condition-output))
        (unless (= 0 condition-code)
          (return)))
      (%collect-execution-result
       (output code)
       (%execute-ast-list-in-context
        context
        (nshell.domain.parsing:while-node-body ast))))))

(defun %execute-case-node-in-context (context ast)
  (let* ((raw-value (nshell.domain.parsing:case-node-value ast))
         (expanded (nshell.domain.expansion:expand-all
                    raw-value
                    (shell-context-environment context)))
         (value (or (first expanded) raw-value)))
    (loop for clause in (nshell.domain.parsing:case-node-clauses ast)
          for pattern = (car clause)
          when (or (string= pattern "*") (string= pattern value))
            do (return (%execute-ast-list-in-context context (cdr clause)))
          finally (return (values nil 0)))))

(defun %execute-sequence-node-in-context (context ast)
  (%with-output-code-accumulator (output code)
    (let* ((commands (nshell.domain.parsing:sequence-node-commands ast))
           (separators (nshell.domain.parsing:sequence-node-separators ast)))
      (loop for command in commands
            for index from 0
            for separator = (and (< index (length separators))
                                 (nth index separators))
            do (cond
                 ((eq :amp separator)
                  (%collect-execution-result
                   (output code)
                   (execute-ast-in-context context command)))
                 (t
                  (%collect-execution-result
                   (output code)
                   (execute-ast-in-context context command))
                  (when (or (and (eq :and separator) (/= code 0))
                            (and (eq :or separator) (= code 0)))
                    (return))))))))

(defun execute-ast-in-context (context ast)
  (cond
    ((nshell.domain.parsing:command-node-p ast)
     (execute-command-node-in-context context ast))
    ((nshell.domain.parsing:pipeline-node-p ast)
     (execute-pipeline-node-in-context context ast))
    ((nshell.domain.parsing:if-node-p ast)
     (%execute-if-node-in-context context ast))
    ((nshell.domain.parsing:for-node-p ast)
     (%execute-for-node-in-context context ast))
    ((nshell.domain.parsing:while-node-p ast)
     (%execute-while-node-in-context context ast))
    ((nshell.domain.parsing:case-node-p ast)
     (%execute-case-node-in-context context ast))
    ((nshell.domain.parsing:begin-end-node-p ast)
     (%execute-ast-list-in-context
      context
      (nshell.domain.parsing:begin-end-node-body ast)))
    ((nshell.domain.parsing:sequence-node-p ast)
     (%execute-sequence-node-in-context context ast))
    (t (values (format nil "source: unsupported syntax~%") 2))))
