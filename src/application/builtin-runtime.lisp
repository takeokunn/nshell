(in-package #:nshell.application)

(defvar *builtin-registry* (make-hash-table :test #'equal)
  "Registry mapping builtin command names to handler functions.")

(defun register-builtin (name handler)
  "Register HANDLER as the builtin implementation for NAME."
  (check-type name string)
  (check-type handler function)
  (setf (gethash name *builtin-registry*) handler))

(defun lookup-builtin (name)
  "Return the builtin handler registered for NAME, or NIL."
  (and name (gethash name *builtin-registry*)))

(defun builtin-p (name)
  "Return true when NAME identifies a registered builtin command."
  (not (null (lookup-builtin name))))

(defun %required-argument-error (builtin option requirement)
  (format nil "~a: ~a requires ~a~%" builtin option requirement))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %with-option-arguments ((remaining option)
                                    on-sentinel
                                    on-unknown
                                    on-operand
                                    &body clauses)
    `(block nil
       (loop while ,remaining
             for ,option = (first ,remaining)
             do (cond
                  ((string= ,option "--")
                   (setf ,remaining (rest ,remaining))
                   ,on-sentinel)
                  ,@clauses
                  ((and (> (length ,option) 1)
                        (char= (char ,option 0) #\-))
                   ,on-unknown)
                  (t ,on-operand)))))

  (defmacro %with-required-argument ((return-target remaining builtin option requirement status) &body body)
    `(if (rest ,remaining)
         (progn ,@body)
         (return-from ,return-target
           (values (%required-argument-error ,builtin ,option ,requirement)
                    ,status)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-plist-accessors (&rest specs)
    `(progn
       ,@(loop for (name key) in specs
               collect `(defun ,name (spec)
                          (getf spec ,key))))))
(defparameter +builtin-string-subcommand-specs+
  '((:name "collect" :handler %builtin-string-collect :manipulation-p t)
    (:name "length" :handler %builtin-string-length)
    (:name "lower" :handler %builtin-string-lower)
    (:name "upper" :handler %builtin-string-upper)
    (:name "join" :handler %builtin-string-join)
    (:name "split" :handler %builtin-string-split)
    (:name "replace" :handler %builtin-string-replace :manipulation-p t)
    (:name "match" :handler %builtin-string-match :manipulation-p t)
    (:name "repeat" :handler %builtin-string-repeat :manipulation-p t)
    (:name "sub" :handler %builtin-string-sub :manipulation-p t)
    (:name "trim" :handler %builtin-string-trim)))

(define-plist-accessors
  (%builtin-string-spec-name :name)
  (%builtin-string-spec-handler :handler)
  (%builtin-string-spec-manipulation-p :manipulation-p))

(defun %builtin-string-specs-for-summary (&key manipulation-only-p)
  (remove-if-not (lambda (spec)
                   (or (not manipulation-only-p)
                       (%builtin-string-spec-manipulation-p spec)))
                 +builtin-string-subcommand-specs+))

(defun %builtin-string-summary (separator &key manipulation-only-p)
  (format nil "string ~a"
          (%string-join
           (mapcar #'%builtin-string-spec-name
                   (%builtin-string-specs-for-summary
                    :manipulation-only-p manipulation-only-p))
           separator)))

(defun %builtin-string-subcommand-summary ()
  (%builtin-string-summary "|"))

(defun %builtin-string-manipulation-summary ()
  (%builtin-string-summary "/" :manipulation-only-p t))

(defparameter +builtin-contains-usage-clauses+
  '("contains [-i|--index] string [values...]"))

(defparameter +builtin-history-usage-clauses+
  '("history [search [--prefix|--contains|--exact|--case-sensitive] query | delete command | clear | size]"))

(defun %builtin-usage-clauses-summary (clauses)
  (format nil "~{~A~^; ~}" clauses))

(defun %builtin-usage (command usage &optional (code 1))
  (values (format nil "~a: usage: ~a~%" command usage) code))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-string-line-builtin (name transform)
    `(defun ,name (context args)
       (declare (ignore context))
       (values (%string-emit-lines args :transform ,transform)
               (if args 0 1)))))

(defun %filesystem-fn (context key)
  (or (getf (shell-context-filesystem-fns context) key)
      (error "Missing filesystem adapter ~s" key)))

(defun %optional-filesystem-fn (context key)
  (getf (shell-context-filesystem-fns context) key))

(defun %process-fn (context key)
  (or (getf (shell-context-process-fns context) key)
      (error "Missing process adapter ~s" key)))

(defun %optional-process-fn (context key)
  (getf (shell-context-process-fns context) key))

(defun %run-external-command-in-context (context command args)
  (let ((capture-runner (%optional-process-fn context :run-external-capture)))
    (if capture-runner
        (funcall capture-runner command args)
        (values nil
                (funcall (%process-fn context :run-external) command args)))))

(defun %path-separator-p (char)
  (or (char= char #\/)
      #+windows (char= char #\\)
      #-windows nil))

(defun %command-has-directory-p (command)
  (position-if #'%path-separator-p command))

(defun %split-path (path)
  (let ((start 0)
        (parts nil))
    (loop for pos = (position #\: path :start start)
          do (push (subseq path start pos) parts)
          while pos
          do (setf start (1+ pos)))
    (nreverse parts)))

(defun %join-path-name (directory command)
  (cond
    ((string= directory "") command)
    ((char= (char directory (1- (length directory))) #\/)
     (concatenate 'string directory command))
    (t (concatenate 'string directory "/" command))))

(defun %stat-path (context path)
  (handler-case
      (funcall (%filesystem-fn context :stat) path)
    (error () nil)))

(defun %path-file-p (context path)
  (let ((fn (%optional-filesystem-fn context :file-exists-p)))
    (cond
      (fn (funcall fn path))
      ((%stat-path context path)
       (let ((pathname (probe-file path)))
         (and pathname (not (uiop:directory-pathname-p pathname)))))
      (t nil))))

(defun %path-directory-p (context path)
  (let ((fn (%optional-filesystem-fn context :directory-exists-p)))
    (cond
      (fn (funcall fn path))
      (t (not (null (uiop:directory-exists-p path)))))))

(defun resolve-command-path (context command)
  "Return COMMAND's executable path from builtins or PATH, or NIL."
  (cond
    ((builtin-p command) (values :builtin command))
    ((%command-has-directory-p command)
     (when (%stat-path context command)
       (values :path command)))
    (t
     (let ((path (or (and (shell-context-environment context)
                          (nshell.domain.environment:env-get
                           (shell-context-environment context) "PATH"))
                     "")))
       (loop for directory in (%split-path path)
             for candidate = (%join-path-name directory command)
             when (%stat-path context candidate)
               do (return (values :path candidate)))))))

(defun %execute-command-by-name-in-context (context command args)
  (multiple-value-bind (function-body function-present-p)
      (gethash command (shell-context-function-table context))
    (let ((handler (lookup-builtin command)))
      (cond
        (function-present-p
         (%source-lines context function-body))
        (handler
         (funcall handler context args))
        (t
         (%run-external-command-in-context context command args))))))

(defparameter +string-flag-option-specs+
  '((:name quiet :short "-q" :long "--quiet")
    (:name all :short "-a" :long "--all")
    (:name ignore-case :short "-i" :long "--ignore-case")))

(defparameter +string-repeat-flag-option-specs+
  '((:name quiet :short "-q" :long "--quiet")
    (:name no-newline :short "-N" :long "--no-newline")))

(defparameter +string-sub-flag-option-specs+
  '((:name quiet :short "-q" :long "--quiet")))

(defparameter +string-repeat-integer-option-specs+
  '((:name count
       :short "-n"
       :long "--count"
       :kind :required
       :short-prefix-length 2
       :long-prefix-length 8)
      (:name max
       :short "-m"
       :long "--max"
       :kind :prefixed
       :short-prefix-length 2
     :long-prefix-length 6)))

(defparameter +string-sub-integer-option-specs+
  '((:name start
     :short "-s"
     :long "--start"
     :kind :prefixed
     :short-prefix-length 2
     :long-prefix-length 8)
    (:name length
     :short "-l"
     :long "--length"
     :kind :prefixed
     :short-prefix-length 2
     :long-prefix-length 9)
    (:name end
     :short "-e"
     :long "--end"
     :kind :prefixed
     :short-prefix-length 2
     :long-prefix-length 6)))

(define-plist-accessors
  (%string-option-spec-name :name)
  (%string-option-spec-short :short)
  (%string-option-spec-long :long)
  (%string-option-spec-kind :kind)
  (%string-option-spec-short-prefix-length :short-prefix-length)
  (%string-option-spec-long-prefix-length :long-prefix-length))

(defun %string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun %string-option-spec-matches-p (option spec)
  (let ((short (%string-option-spec-short spec))
        (long (%string-option-spec-long spec)))
    (case (%string-option-spec-kind spec)
      (:prefixed
       (or (%string-prefix-p short option)
           (%string-prefix-p long option)))
      (:required
       (or (string= option short)
           (string= option long)
           (%string-prefix-p short option)
           (%string-prefix-p long option)))
        (t
         (or (string= option short)
             (string= option long))))))

(defun %string-find-option-spec (option specs)
  (find-if (lambda (spec)
             (%string-option-spec-matches-p option spec))
            specs))

(defun %string-parse-integer-option-spec (option remaining spec)
  (let* ((short (%string-option-spec-short spec))
         (long (%string-option-spec-long spec))
         (short-prefix-length (%string-option-spec-short-prefix-length spec))
         (long-prefix-length (%string-option-spec-long-prefix-length spec))
         (attached-value
           (cond
             ((and short-prefix-length
                   (%string-prefix-p short option)
                   (> (length option) short-prefix-length))
              (subseq option short-prefix-length))
             ((and long-prefix-length
                   (%string-prefix-p long option)
                   (>= (length option) long-prefix-length)
                   (char= (char option (1- long-prefix-length)) #\=))
              (subseq option long-prefix-length))
             (t nil)))
         (separate-value (and (null attached-value)
                              (rest remaining)
                              (second remaining))))
    (cond
      (attached-value
       (multiple-value-bind (parsed error)
           (%string-parse-integer-option option attached-value)
         (if error
             (values nil remaining error)
             (values parsed (rest remaining) nil))))
      (separate-value
       (multiple-value-bind (parsed error)
           (%string-parse-integer-option option separate-value)
         (if error
             (values nil remaining error)
             (values parsed (cddr remaining) nil))))
      (t
       (values nil remaining
               (%required-argument-error "string" option "an integer"))))))

(defun %string-parse-integer-option (option value)
  (handler-case
      (values (parse-integer value :junk-allowed nil) nil)
    (error ()
      (values nil (format nil "string: invalid integer for ~a: ~a~%" option value)))))

(defun %string-option-argument-p (option)
  (and option
       (>= (length option) 2)
       (char= (char option 0) #\-)))

(defun %string-parse-option-stream (remaining builtin flag-specs integer-specs on-flag on-integer)
  (loop while remaining
        for option = (first remaining)
        do (cond
             ((string= option "--")
              (setf remaining (rest remaining))
              (return))
             ((not (%string-option-argument-p option))
              (return))
             (t
              (let ((flag-spec (%string-find-option-spec option flag-specs))
                    (integer-spec (%string-find-option-spec option integer-specs)))
                (cond
                  (flag-spec
                   (funcall on-flag (%string-option-spec-name flag-spec)
                            remaining)
                   (setf remaining (rest remaining)))
                  (integer-spec
                   (multiple-value-bind (parsed next-remaining error)
                       (%string-parse-integer-option-spec option remaining integer-spec)
                     (when error
                       (return-from %string-parse-option-stream
                         (values remaining error)))
                     (setf remaining
                           (funcall on-integer (%string-option-spec-name integer-spec)
                                    parsed next-remaining))))
                  (t
                   (return-from %string-parse-option-stream
                     (values remaining
                             (format nil "~a: unknown option ~a~%"
                                     builtin option)))))))))
  (values remaining nil))


(defun %string-empty-p (string)
  (zerop (length string)))

(defun %string-trim-trailing-newlines (string)
  (let ((end (length string)))
    (loop while (and (> end 0)
                     (char= (char string (1- end)) #\Newline))
          do (decf end))
    (subseq string 0 end)))

(defun %string-lines (string)
  (let ((start 0)
        (lines nil))
    (loop for pos = (position #\Newline string :start start)
          do (push (subseq string start pos) lines)
          while pos
          do (setf start (1+ pos)))
    (nreverse lines)))

(defun %string-join (strings separator)
  (with-output-to-string (out)
    (when strings
      (write-string (first strings) out)
      (dolist (string (rest strings))
        (write-string separator out)
        (write-string string out)))))

(defun %join-command-args (args)
  (%string-join args " "))

(defun %string-split-on (separator string)
  (cond
    ((%string-empty-p separator)
     (map 'list #'string string))
    (t
     (let ((start 0)
           (parts nil)
           (separator-length (length separator)))
       (loop for pos = (search separator string :start2 start)
             do (push (subseq string start pos) parts)
             while pos
             do (setf start (+ pos separator-length)))
       (nreverse parts)))))

(defun %string-repeat-text (text count &optional max-length)
  (when (plusp count)
    (let ((repeated (with-output-to-string (out)
                      (loop repeat count do
                          (write-string text out)))))
      (if max-length
          (subseq repeated 0 (min (length repeated) max-length))
          repeated))))

(defun %string-emit-lines (lines &key (transform #'identity))
  (with-output-to-string (out)
    (dolist (line lines)
      (write-string (funcall transform line) out)
      (write-char #\Newline out))))

(defun %string-collect-texts (texts quiet-p collector)
  (let ((matched-p nil))
    (values
     (with-output-to-string (out)
       (dolist (text texts)
         (multiple-value-bind (line text-matched-p)
             (funcall collector text)
           (when text-matched-p
             (setf matched-p t)
             (unless quiet-p
               (write-string line out)
               (write-char #\Newline out))))))
     (if matched-p 0 1))))

(defun %string-case-test (ignore-case)
  (if ignore-case #'char-equal #'char=))

(defun %string-wildcard-match-p (pattern string &key ignore-case)
  (let ((test (if ignore-case #'char-equal #'char=))
        (pattern-length (length pattern))
        (string-length (length string))
        (memo (make-hash-table :test #'equal)))
    (labels ((match (pattern-index string-index)
               (or (gethash (cons pattern-index string-index) memo)
                   (setf (gethash (cons pattern-index string-index) memo)
                         (cond
                           ((= pattern-index pattern-length)
                            (= string-index string-length))
                           ((char= (char pattern pattern-index) #\*)
                            (or (match (1+ pattern-index) string-index)
                                (and (< string-index string-length)
                                     (match pattern-index (1+ string-index)))))
                           ((char= (char pattern pattern-index) #\?)
                            (and (< string-index string-length)
                                 (match (1+ pattern-index) (1+ string-index))))
                           (t
                            (and (< string-index string-length)
                                 (funcall test (char pattern pattern-index)
                                          (char string string-index))
                                 (match (1+ pattern-index) (1+ string-index)))))))))
      (match 0 0))))

(defun %string-replace-text (text pattern replacement &key all ignore-case)
  (let ((test (%string-case-test ignore-case))
        (pattern-length (length pattern)))
    (if (%string-empty-p pattern)
        (values text nil)
        (if all
            (let ((matched-p nil))
              (values
               (with-output-to-string (out)
                 (loop with start = 0
                       for pos = (search pattern text :start2 start :test test)
                       while pos
                       do (setf matched-p t)
                          (write-string (subseq text start pos) out)
                          (write-string replacement out)
                          (setf start (+ pos pattern-length))
                       finally (write-string (subseq text start) out)))
               matched-p))
            (let ((pos (search pattern text :start2 0 :test test)))
              (if pos
                  (values (concatenate 'string
                                       (subseq text 0 pos)
                                       replacement
                                       (subseq text (+ pos pattern-length)))
                          t)
                  (values text nil)))))))

(defun %string-normalize-start (start length)
  (if (minusp start)
      (+ length start 1)
      start))

(defun %string-normalize-end (end length)
  (if (minusp end)
      (+ length end)
      end))

(defun %string-slice (text start &key length end)
  (let* ((text-length (length text))
         (start-index (max 0 (1- (%string-normalize-start start text-length))))
         (end-position (cond
                         (length
                          (+ (%string-normalize-start start text-length)
                             length -1))
                         (end
                          (%string-normalize-end end text-length))
                         (t
                          text-length)))
         (end-index (min text-length (max 0 end-position))))
    (if (and (< start-index text-length)
             (>= end-index start-index))
        (subseq text start-index end-index)
        "")))

(defun %string-repeat-effective-count (text count max-length)
  (or count
      (and max-length
           (max 1 (ceiling max-length (max 1 (length text)))))
      1))

(defun %builtin-string-collect (context args)
  (declare (ignore context))
  (let ((allow-empty-p nil)
        (preserve-newlines-p nil)
        (remaining args))
    (block parse
      (%with-option-arguments (remaining option)
          (return)
          (return-from %builtin-string-collect
            (values (format nil "string: unknown option ~a~%" option) 1))
          (return)
        ((or (string= option "-N")
             (string= option "--no-newline"))
         (setf preserve-newlines-p t
               remaining (rest remaining)))
        ((string= option "--allow-empty")
         (setf allow-empty-p t
               remaining (rest remaining)))))
    (let ((has-non-empty nil)
          (lines nil))
      (dolist (text remaining)
        (dolist (line (%string-lines
                       (if preserve-newlines-p
                           text
                           (%string-trim-trailing-newlines text))))
          (unless (and (not preserve-newlines-p)
                       (not allow-empty-p)
                       (%string-empty-p line))
            (unless (%string-empty-p line)
              (setf has-non-empty t))
            (push line lines))))
      (values (%string-emit-lines (nreverse lines))
              (if has-non-empty 0 1)))))

(define-string-line-builtin %builtin-string-length
  (lambda (text)
    (princ-to-string (length text))))

(define-string-line-builtin %builtin-string-lower #'string-downcase)

(define-string-line-builtin %builtin-string-upper #'string-upcase)

(defun %builtin-string-join (context args)
  (declare (ignore context))
  (if (rest args)
      (values (format nil "~a~%"
                      (%string-join (rest args) (first args)))
              0)
      (%builtin-usage "string" (%builtin-string-subcommand-summary))))

(defun %builtin-string-split (context args)
  (declare (ignore context))
  (if (rest args)
      (values (%string-emit-lines
               (loop for text in (rest args)
                     append (%string-split-on (first args) text)))
              0)
      (%builtin-usage "string" (%builtin-string-subcommand-summary))))

(defun %parse-string-flags (remaining builtin)
  (let ((quiet-p nil)
        (all-p nil)
        (ignore-case-p nil))
    (multiple-value-bind (remaining error)
        (%string-parse-option-stream
         remaining builtin
         +string-flag-option-specs+ nil
         (lambda (name remaining)
           (case name
             (quiet
              (setf quiet-p t))
             (all
              (setf all-p t))
             (ignore-case
              (setf ignore-case-p t)))
           remaining)
         (lambda (name parsed next-remaining)
           (declare (ignore name parsed))
           next-remaining))
      (values quiet-p all-p ignore-case-p remaining error))))

(defun %builtin-string-replace (context args)
  (declare (ignore context))
  (multiple-value-bind (quiet-p all-p ignore-case-p remaining error)
      (%parse-string-flags args "string")
    (when error
      (return-from %builtin-string-replace (values error 1)))
    (if (< (length remaining) 3)
        (%builtin-usage "string" (%builtin-string-subcommand-summary))
        (let ((pattern (first remaining))
              (replacement (second remaining))
              (values (cddr remaining)))
          (%string-collect-texts
           values
           quiet-p
           (lambda (text)
             (%string-replace-text text pattern replacement
                                   :all all-p
                                   :ignore-case ignore-case-p)))))))

(defun %builtin-string-match (context args)
  (declare (ignore context))
  (multiple-value-bind (quiet-p _all-p ignore-case-p remaining error)
      (%parse-string-flags args "string")
    (declare (ignore _all-p))
    (when error
      (return-from %builtin-string-match (values error 1)))
    (if (< (length remaining) 2)
        (%builtin-usage "string" (%builtin-string-subcommand-summary))
        (let ((pattern (first remaining))
              (values (rest remaining)))
          (%string-collect-texts
           values
           quiet-p
           (lambda (text)
             (if (%string-wildcard-match-p pattern text
                                            :ignore-case ignore-case-p)
                 (values text t)
                 (values nil nil))))))))

(defun %builtin-string-repeat (context args)
  (declare (ignore context))
  (let ((repeat-count nil)
        (max-length nil)
        (quiet-p nil)
        (no-newline-p nil)
        (remaining args))
    (multiple-value-bind (next-remaining error)
        (%string-parse-option-stream
         remaining "string"
         +string-repeat-flag-option-specs+
         +string-repeat-integer-option-specs+
         (lambda (name remaining)
           (case name
             (quiet
              (setf quiet-p t))
             (no-newline
              (setf no-newline-p t)))
           remaining)
         (lambda (name parsed next-remaining)
           (case name
             (count
              (setf repeat-count parsed))
             (max
              (setf max-length parsed)))
           next-remaining))
      (when error
        (return-from %builtin-string-repeat (values error 1)))
      (setf remaining next-remaining))
    (when (and remaining (null repeat-count) (null max-length))
      (multiple-value-bind (parsed error)
          (%string-parse-integer-option "count" (first remaining))
        (declare (ignore error))
        (when parsed
          (setf repeat-count parsed
                remaining (rest remaining)))))
    (if (or (null remaining)
            (not (and (plusp (or repeat-count 1))
                      (or (null max-length) (plusp max-length)))))
        (values "" 1)
        (values
         (with-output-to-string (out)
           (loop for texts on remaining
                 for text = (first texts)
                 do (let* ((repeat-count (%string-repeat-effective-count
                                           text repeat-count max-length))
                           (repeated (%string-repeat-text text repeat-count max-length)))
                      (when repeated
                        (unless quiet-p
                          (write-string repeated out))
                        (unless (or quiet-p
                                    (and no-newline-p (null (rest texts))))
                          (write-char #\Newline out))))))
         0))))

(defun %parse-string-sub-options (remaining)
  (let ((start 1)
        (length nil)
        (end nil)
        (quiet-p nil))
    (multiple-value-bind (remaining error)
        (%string-parse-option-stream
         remaining "string"
         +string-sub-flag-option-specs+
         +string-sub-integer-option-specs+
         (lambda (name remaining)
           (declare (ignore name))
           (setf quiet-p t)
           remaining)
         (lambda (name parsed next-remaining)
           (case name
             (start
              (setf start parsed))
             (length
              (setf length parsed))
             (end
              (setf end parsed)))
           next-remaining))
      (values start length end quiet-p remaining error))))

(defun %builtin-string-sub (context args)
  (declare (ignore context))
  (multiple-value-bind (start length end quiet-p remaining error)
      (%parse-string-sub-options args)
    (when error
      (return-from %builtin-string-sub (values error 1)))
    (cond
      ((and length end)
       (values "string: -l and -e are mutually exclusive~%" 1))
      ((null remaining)
       (%builtin-usage "string" (%builtin-string-subcommand-summary)))
      (t
       (values
        (with-output-to-string (out)
          (dolist (text remaining)
            (let ((slice (%string-slice text start
                                        :length length
                                        :end end)))
              (unless quiet-p
                (write-string slice out)
                (write-char #\Newline out)))))
        0)))))

(define-string-line-builtin %builtin-string-trim
  (lambda (text)
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 text)))

(defun %builtin-string-dispatch (args)
  (let* ((subcommand (first args))
         (spec (find subcommand +builtin-string-subcommand-specs+
                     :key #'%builtin-string-spec-name
                     :test #'string=))
         (handler (%builtin-string-spec-handler spec)))
    (if handler
        (funcall handler nil (rest args))
        (%builtin-usage "string" (%builtin-string-subcommand-summary)))))
