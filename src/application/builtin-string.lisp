(in-package #:nshell.application)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-plist-accessors (&rest specs)
    `(progn
       ,@(loop for (name key) in specs
               collect `(defun ,name (spec)
                          (getf spec ,key)))))

  (defmacro define-string-line-builtin (name transform)
    `(defun ,name (context args)
       (declare (ignore context))
       (values (%string-emit-lines args :transform ,transform)
               (if args 0 1)))))

(define-plist-accessors
  (%builtin-string-spec-name :name)
  (%builtin-string-spec-handler :handler)
  (%builtin-string-spec-manipulation-p :manipulation-p)
  (%string-option-spec-name :name)
  (%string-option-spec-short :short)
  (%string-option-spec-long :long)
  (%string-option-spec-kind :kind)
  (%string-option-spec-short-prefix-length :short-prefix-length)
  (%string-option-spec-long-prefix-length :long-prefix-length))

(defun %builtin-string-summary (separator &key manipulation-only-p)
  (format nil "string ~a"
          (%string-join
           (mapcar #'%builtin-string-spec-name
                   (remove-if-not (lambda (spec)
                                    (or (not manipulation-only-p)
                                        (%builtin-string-spec-manipulation-p spec)))
                                  +builtin-string-subcommand-specs+))
           separator)))

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

(defun %string-parse-integer-option (option value)
  (handler-case
      (values (parse-integer value :junk-allowed nil) nil)
    (error ()
      (values nil (format nil "string: invalid integer for ~a: ~a~%" option value)))))

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
    (labels ((parse-value (value next-remaining)
               (multiple-value-bind (parsed error)
                   (%string-parse-integer-option option value)
                 (if error
                     (values nil remaining error)
                     (values parsed next-remaining nil)))))
      (cond
        (attached-value
         (parse-value attached-value (rest remaining)))
        (separate-value
         (parse-value separate-value (cddr remaining)))
        (t
         (values nil remaining
                 (%required-argument-error "string" option "an integer")))))))

(defun %string-option-argument-p (option)
  (and option
       (>= (length option) 2)
       (char= (char option 0) #\-)))

(defun %string-parse-option-stream (remaining builtin flag-specs integer-specs on-flag on-integer)
  (labels ((advance-flag (spec)
             (funcall on-flag (%string-option-spec-name spec) remaining)
             (rest remaining))
           (advance-integer (option spec)
             (multiple-value-bind (parsed next-remaining error)
                 (%string-parse-integer-option-spec option remaining spec)
               (when error
                 (return-from %string-parse-option-stream
                   (values remaining error)))
               (funcall on-integer (%string-option-spec-name spec)
                        parsed next-remaining)))
           (unknown-option (option)
             (return-from %string-parse-option-stream
               (values remaining
                       (format nil "~a: unknown option ~a~%"
                               builtin option)))))
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
                     (setf remaining (advance-flag flag-spec)))
                    (integer-spec
                     (setf remaining (advance-integer option integer-spec)))
                    (t
                     (unknown-option option))))))))
  (values remaining nil))

(defun %string-empty-p (string)
  (zerop (length string)))

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

(defun %builtin-string-collect (context args)
  (declare (ignore context))
  (let ((allow-empty-p nil)
        (preserve-newlines-p nil)
        (remaining args))
    (multiple-value-bind (next-remaining error)
        (%string-parse-option-stream
         remaining "string"
         +string-collect-flag-option-specs+ nil
         (lambda (name remaining)
           (case name
             (allow-empty
              (setf allow-empty-p t))
             (no-newline
              (setf preserve-newlines-p t)))
           (setf remaining (rest remaining))
           remaining)
         (lambda (name parsed next-remaining)
           (declare (ignore name parsed))
           next-remaining))
      (when error
        (return-from %builtin-string-collect (values error 1)))
      (setf remaining next-remaining))
    (let ((has-non-empty nil)
          (lines nil))
      (labels ((trim-trailing-newlines (string)
                 (let ((end (length string)))
                   (loop while (and (> end 0)
                                    (char= (char string (1- end)) #\Newline))
                         do (decf end))
                   (subseq string 0 end))))
        (dolist (text remaining)
          (dolist (line (%string-lines
                         (if preserve-newlines-p
                             text
                             (trim-trailing-newlines text))))
            (unless (and (not preserve-newlines-p)
                         (not allow-empty-p)
                         (%string-empty-p line))
              (unless (%string-empty-p line)
                (setf has-non-empty t))
              (push line lines))))
        (values (%string-emit-lines (nreverse lines))
                (if has-non-empty 0 1))))))

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
      (%builtin-usage "string" (%builtin-string-summary "|"))))

(defun %builtin-string-split (context args)
  (declare (ignore context))
  (if (rest args)
      (labels ((split-on (separator string)
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
                      (nreverse parts))))))
        (values (%string-emit-lines
                 (loop for text in (rest args)
                       append (split-on (first args) text)))
                0))
      (%builtin-usage "string" (%builtin-string-summary "|"))))

(defun %parse-string-flags (remaining builtin flag-specs)
  (let ((quiet-p nil)
        (all-p nil)
        (ignore-case-p nil))
    (multiple-value-bind (remaining error)
        (%string-parse-option-stream
         remaining builtin
         flag-specs nil
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

(defun %string-pattern-builtin (args flag-specs required-args collector)
  (multiple-value-bind (quiet-p all-p ignore-case-p remaining error)
      (%parse-string-flags args "string" flag-specs)
    (when error
      (return-from %string-pattern-builtin (values error 1)))
    (if (< (length remaining) required-args)
        (%builtin-usage "string" (%builtin-string-summary "|"))
        (funcall collector quiet-p all-p ignore-case-p remaining))))

(defun %builtin-string-replace (context args)
  (declare (ignore context))
  (%string-pattern-builtin
   args +string-replace-flag-option-specs+ 3
   (lambda (quiet-p all-p ignore-case-p remaining)
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
  (%string-pattern-builtin
   args +string-match-flag-option-specs+ 2
   (lambda (quiet-p all-p ignore-case-p remaining)
     (declare (ignore all-p))
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

(defun %string-repeat-effective-count (text count max-length)
  (or count
      (and max-length
           (max 1 (ceiling max-length (max 1 (length text)))))
      1))

(defun %string-repeat-text (text count &optional max-length)
  (when (plusp count)
    (let ((repeated (with-output-to-string (out)
                      (loop repeat count do
                        (write-string text out)))))
      (if max-length
          (subseq repeated 0 (min (length repeated) max-length))
          repeated))))

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
    (when (and (null repeat-count)
               (null max-length)
               (rest remaining))
      (multiple-value-bind (parsed error)
          (%string-parse-integer-option "count" (first remaining))
        (declare (ignore error))
        (when parsed
          (return-from %builtin-string-repeat
            (%builtin-usage "string" (%builtin-string-summary "|"))))))
    (if (or (null remaining)
            (not (and (plusp (or repeat-count 1))
                      (or (null max-length) (plusp max-length)))))
        (values "" 1)
        (values
         (with-output-to-string (out)
           (loop for texts on remaining
                 for text = (first texts)
                 for effective-count = (%string-repeat-effective-count
                                         text repeat-count max-length)
                 for repeated = (%string-repeat-text text effective-count max-length)
                 do (when repeated
                      (unless quiet-p
                        (write-string repeated out))
                      (unless (or quiet-p
                                  (and no-newline-p (null (rest texts))))
                        (write-char #\Newline out)))))
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

(defun %string-sub-normalize-start (start length)
  (if (minusp start)
      (+ length start 1)
      start))

(defun %string-sub-normalize-end (end length)
  (if (minusp end)
      (+ length end)
      end))

(defun %string-slice (text start &key length end)
  (let* ((text-length (length text))
         (start-index (max 0 (1- (%string-sub-normalize-start start text-length))))
         (end-position (cond
                         (length
                          (+ (%string-sub-normalize-start start text-length)
                             length -1))
                         (end
                          (%string-sub-normalize-end end text-length))
                         (t
                          text-length)))
         (end-index (min text-length (max 0 end-position))))
    (if (and (< start-index text-length)
             (>= end-index start-index))
        (subseq text start-index end-index)
        "")))

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
       (%builtin-usage "string" (%builtin-string-summary "|")))
      (t
       (values
        (with-output-to-string (out)
          (dolist (text remaining)
            (let ((slice (%string-slice text start :length length :end end)))
              (unless quiet-p
                (write-string slice out)
                (write-char #\Newline out)))))
        0)))))

(define-string-line-builtin %builtin-string-trim
  (lambda (text)
    (string-trim '(#\Space #\Tab #\Newline #\Return)
                 text)))

(defun %builtin-string-dispatch (context args)
  (let* ((subcommand (first args))
         (spec (find subcommand +builtin-string-subcommand-specs+
                     :key #'%builtin-string-spec-name
                     :test #'string=))
         (handler (%builtin-string-spec-handler spec)))
    (if handler
        (funcall handler context (rest args))
        (%builtin-usage "string" (%builtin-string-summary "|")))))
