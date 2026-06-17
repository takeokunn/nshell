(in-package #:nshell.domain.parsing)

(defparameter +control-flow-keywords+
  '("if" "else" "for" "in" "while" "case" "switch" "begin" "end"))

(defun control-flow-keyword-p (value)
  (and (stringp value)
       (not (null (member value +control-flow-keywords+ :test #'string=)))))

(defun %command-keyword (node)
  (when (command-node-p node)
    (let ((command (command-node-command node)))
      (and (control-flow-keyword-p command) command))))

(defun %block-opening-keyword-p (keyword)
  (not (null (member keyword '("if" "for" "while" "case" "switch" "begin")
                     :test #'string=))))

(defun %command-first-arg-value (header &optional (default ""))
  (let ((args (and (command-node-p header) (command-node-args header))))
    (if args
        (arg-value (first args))
        default)))

(defun %command-from-header-args (header)
  (let ((args (command-node-args header)))
    (when args
      (make-command-node (arg-value (first args)) (rest args)))))

(defun %consume-control-flow-terminator (nodes keyword)
  (if (and nodes (string= (%command-keyword (first nodes)) keyword))
      (rest nodes)
      nodes))

(defun %stack-top-keyword (stack)
  (let ((top (first stack)))
    (cond
      ((stringp top) top)
      ((consp top) (getf top :keyword))
      (t nil))))

(defun %case-within-switch-p (keyword stack)
  (and keyword
       (string= keyword "case")
       (string= (%stack-top-keyword stack) "switch")))

(defun %unclosed-control-flow-p (cmds)
  (loop with stack = nil
        for cmd in cmds
        for keyword = (%command-keyword cmd)
        do (cond
             ((and keyword
                   (string= keyword "case")
                   (%case-within-switch-p keyword stack)))
             ((and keyword
                   (string= keyword "case"))
              nil)
             ((and keyword (%block-opening-keyword-p keyword))
              (push keyword stack))
             ((and keyword (string= keyword "end") stack)
              (pop stack)))
        finally (return (not (null stack)))))

(defun %command-diagnostic-span (node input-length)
  (let ((span (and (ast-node-p node) (ast-node-span node))))
    (if (and (consp span) (consp (rest span)))
        (values (first span) (second span))
        (values input-length input-length))))

(defun %push-control-flow-diagnostic (diagnostics node keyword input-length)
  (multiple-value-bind (start end)
      (%command-diagnostic-span node input-length)
    (push (make-parse-diagnostic
           :unexpected-control-flow
           (format nil "Unexpected '~a'" keyword)
           start
           end)
          diagnostics)))

(defun %control-flow-stack-keyword (stack)
  (getf (first stack) :keyword))

(defun %control-flow-stack-else-seen-p (stack)
  (getf (first stack) :else-seen))

(defun %mark-control-flow-stack-else-seen (stack)
  (setf (getf (first stack) :else-seen) t))

(defun %push-control-flow-frame (stack keyword)
  (push (list :keyword keyword :else-seen nil) stack))

(defun %unexpected-control-flow-diagnostics (cmds input-length)
  (let ((stack nil)
        (diagnostics nil))
    (dolist (cmd cmds)
      (let ((keyword (%command-keyword cmd)))
        (cond
          ((and keyword
                (string= keyword "case")
                (%case-within-switch-p keyword stack)))
          ((and keyword
                (string= keyword "case"))
           (setf diagnostics
                 (%push-control-flow-diagnostic diagnostics cmd keyword input-length)))
          ((and keyword (%block-opening-keyword-p keyword))
           (setf stack (%push-control-flow-frame stack keyword)))
          ((and keyword (string= keyword "else"))
           (if (and stack
                    (string= (%control-flow-stack-keyword stack) "if")
                    (not (%control-flow-stack-else-seen-p stack)))
               (%mark-control-flow-stack-else-seen stack)
               (setf diagnostics
                     (%push-control-flow-diagnostic diagnostics cmd keyword input-length))))
          ((and keyword (string= keyword "end"))
           (if stack
               (pop stack)
               (setf diagnostics
                     (%push-control-flow-diagnostic diagnostics cmd keyword input-length)))))))
    (nreverse diagnostics)))

(defun %group-control-flow-body (nodes terminators)
  (let ((body nil)
        (remaining nodes)
        (stop nil))
    (loop while remaining
          for node = (first remaining)
          for keyword = (%command-keyword node)
          do (if (and keyword (member keyword terminators :test #'string=))
                 (progn
                   (setf stop keyword)
                   (return))
                 (multiple-value-bind (parsed rest)
                     (%group-control-flow-next remaining)
                   (push parsed body)
                   (setf remaining rest))))
    (values (nreverse body) remaining stop)))

(defun %group-control-flow-clauses (nodes clause-parser)
  (let ((clauses nil)
        (remaining (rest nodes)))
    (loop while remaining
          for keyword = (%command-keyword (first remaining))
          do (cond
               ((and keyword (string= keyword "end"))
                (setf remaining (rest remaining))
                (return))
               (t
                (multiple-value-bind (new-clauses rest)
                    (funcall clause-parser remaining)
                  (dolist (clause new-clauses)
                    (push clause clauses))
                  (setf remaining rest)))))
    (values (nreverse clauses) remaining)))

(defun %group-control-flow-with-end-body (nodes builder)
  (multiple-value-bind (body rest)
      (%group-control-flow-body (rest nodes) '("end"))
    (values (funcall builder body)
            (%consume-control-flow-terminator rest "end"))))

(defun %group-control-flow-if-then (condition then-branch rest)
  (values (make-if-node condition then-branch)
          (%consume-control-flow-terminator rest "end")))

(defun %group-control-flow-if-else (condition then-branch rest)
  (multiple-value-bind (else-branch after-else)
      (%group-control-flow-body (rest rest) '("end"))
    (values (make-if-node condition then-branch else-branch)
            (%consume-control-flow-terminator after-else "end"))))

(defun %group-control-flow-if (nodes)
  (let* ((header (first nodes))
         (condition (%command-from-header-args header)))
    (multiple-value-bind (then-branch rest stop)
        (%group-control-flow-body (rest nodes) '("else" "end"))
      (cond
        ((and stop (string= stop "else"))
         (%group-control-flow-if-else condition then-branch rest))
        ((and stop (string= stop "end"))
         (%group-control-flow-if-then condition then-branch rest))
        (t (values (make-if-node condition then-branch) rest))))))

(defun %group-control-flow-case-clause (nodes)
  (let ((pattern (%command-first-arg-value (first nodes) "*")))
    (multiple-value-bind (body rest)
        (%group-control-flow-body (rest nodes) '("end"))
      (values (list (cons pattern body)) rest))))

(defun %command-arg-values (node)
  (mapcar #'arg-value (command-node-args node)))

(defun %group-control-flow-for (nodes)
  (let* ((header (first nodes))
         (args (command-node-args header))
         (var-name (%command-first-arg-value header))
         (in-pos (position "in" args :test (lambda (item arg)
                                             (string= item (arg-value arg)))))
         (in-values (if in-pos (subseq args (1+ in-pos)) (rest args))))
    (%group-control-flow-with-end-body
     nodes
     (lambda (body)
       (make-for-node var-name in-values body)))))

(defun %group-control-flow-while (nodes)
  (let ((condition (%command-from-header-args (first nodes))))
    (%group-control-flow-with-end-body
     nodes
     (lambda (body)
       (make-while-node condition body)))))

(defun %group-control-flow-case (nodes)
  (let* ((header (first nodes))
         (value (%command-first-arg-value header)))
    (multiple-value-bind (clauses remaining)
        (%group-control-flow-clauses nodes #'%group-control-flow-case-clause)
      (values (make-case-node value clauses) remaining))))

(defun %group-control-flow-switch-clause (nodes)
  (let* ((header (first nodes))
         (keyword (%command-keyword header)))
    (if (and keyword (string= keyword "case"))
        (%group-control-flow-switch-case-clause nodes)
        (%group-control-flow-switch-default-clause nodes))))

(defun %group-control-flow-switch-case-clause (nodes)
  (let ((patterns (or (%command-arg-values (first nodes))
                      '("*"))))
    (multiple-value-bind (body rest)
        (%group-control-flow-body (rest nodes) '("case" "end"))
      (values (mapcar (lambda (pattern)
                        (cons pattern body))
                      patterns)
              rest))))

(defun %group-control-flow-switch-default-clause (nodes)
  (multiple-value-bind (body rest)
      (%group-control-flow-body nodes '("case" "end"))
    (values (list (cons "*" body)) rest)))

(defun %group-control-flow-switch (nodes)
  (let* ((header (first nodes))
         (value (%command-first-arg-value header)))
    (multiple-value-bind (clauses remaining)
        (%group-control-flow-clauses nodes #'%group-control-flow-switch-clause)
      (values (make-case-node value clauses) remaining))))

(defun %group-control-flow-begin (nodes)
  (%group-control-flow-with-end-body
   nodes
   (lambda (body)
     (make-begin-end-node body))))

(defun %group-control-flow-next (nodes)
  (let* ((node (first nodes))
         (keyword (%command-keyword node)))
    (cond
      ((and keyword (string= keyword "if")) (%group-control-flow-if nodes))
      ((and keyword (string= keyword "for")) (%group-control-flow-for nodes))
      ((and keyword (string= keyword "while")) (%group-control-flow-while nodes))
      ((and keyword (string= keyword "case")) (%group-control-flow-case nodes))
      ((and keyword (string= keyword "switch")) (%group-control-flow-switch nodes))
      ((and keyword (string= keyword "begin")) (%group-control-flow-begin nodes))
      (t (values (group-control-flow node) (rest nodes))))))

(defun group-control-flow (ast)
  (cond
    ((sequence-node-p ast)
     (multiple-value-bind (commands rest stop)
         (%group-control-flow-body (sequence-node-commands ast) nil)
       (declare (ignore stop))
       (declare (ignore rest))
       (let ((separators (sequence-node-separators ast)))
         (if (and (= (length commands) 1)
                  (not (eq :amp (first separators))))
             (first commands)
             (make-sequence-node commands separators)))))
    ((pipeline-node-p ast)
     (make-pipeline-node (mapcar #'group-control-flow (pipeline-node-commands ast))))
    (t ast)))
