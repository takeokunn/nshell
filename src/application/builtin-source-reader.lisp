(in-package #:nshell.application)

(defun %append-source-continuation (text line result)
  (concatenate 'string
               text
               (if (nshell.domain.parsing:parse-diagnostic-kind-p
                    result :trailing-continuation)
                   " "
                   "; ")
               line))

(defun %parse-source-line (source)
  (nshell.domain.parsing:with-parsed-command-line-case (result ast source)
    (:complete
     result)
    (:error
     result)
    (:incomplete
     result)))

(defun %source-substitution-fallback (source)
  (list (format nil "(~a)" source)))

(defun %collect-source-form (line remaining)
  (let ((text line)
        (tail remaining)
        (result (%parse-source-line line)))
    (loop while (and tail
                     (or (nshell.domain.parsing:parse-diagnostic-kind-p
                          result :unclosed-block)
                         (nshell.domain.parsing:parse-diagnostic-kind-p
                          result :trailing-continuation)))
          do (setf text (%append-source-continuation text (pop tail) result)
                   result (%parse-source-line text)))
    (values text tail)))

(defun %collect-source-lines (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect line))

(defun %expand-source-arg (arg &optional environment)
  (let ((value (nshell.domain.parsing:arg-value arg)))
    (cond
      ((or (null environment)
           (nshell.domain.parsing:arg-quoted-p arg))
       (list value))
      ((eq (nshell.domain.parsing:arg-quote-style arg) :double)
       (list (nshell.domain.expansion:expand-double-quoted value environment)))
      (t
       (nshell.domain.expansion:expand-all value environment)))))

(defun %trim-command-substitution-output (output)
  (let* ((text (or output ""))
         (end (length text)))
    (loop while (and (> end 0)
                     (member (char text (1- end)) '(#\Newline #\Return)))
          do (decf end))
    (subseq text 0 end)))

(defun %command-substitution-fields (output)
  (let ((text (%trim-command-substitution-output output))
        (fields nil)
        (start 0))
    (unless (string= text "")
      (loop for newline = (position #\Newline text :start start)
            do (push (subseq text start newline) fields)
            if newline
              do (setf start (1+ newline))
            else
              do (return)))
    (nreverse fields)))

(defun %command-substitution-end (value start)
  (let ((depth 0)
        (quote nil)
        (escaped nil))
    (loop for index from start below (length value)
          for ch = (char value index)
          do (cond
               (escaped
                (setf escaped nil))
               ((char= ch #\\)
                (setf escaped t))
               (quote
                (when (char= ch quote)
                  (setf quote nil)))
               ((or (char= ch #\') (char= ch #\"))
                (setf quote ch))
               ((char= ch #\()
                (incf depth))
               ((char= ch #\))
                (decf depth)
                (when (zerop depth)
                  (return index)))))))

(defun %append-command-substitution-char (parts ch)
  (mapcar (lambda (part)
            (concatenate 'string part (string ch)))
          parts))

(defun %append-command-substitution-fields (parts fields)
  (let ((result nil)
        (values (or fields '(""))))
    (dolist (part parts (nreverse result))
      (dolist (field values)
        (push (concatenate 'string part field) result)))))

(defun %execute-command-substitution-fields (context source)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) source)))
    (if (string= trimmed "")
        nil
        (nshell.domain.parsing:with-parsed-command-line-case (result ast trimmed)
          (:complete
           (if ast
               (multiple-value-bind (output code)
                   (execute-ast-in-context context ast)
                 (declare (ignore code))
                 (%command-substitution-fields output))
               (%source-substitution-fallback source)))
          (:error
           (%source-substitution-fallback source))
          (:incomplete
           (%source-substitution-fallback source))))))

(defun %append-command-substitution-string (parts string)
  (mapcar (lambda (part) (concatenate 'string part string)) parts))

(defun %paren-balanced-end (value start)
  "VALUE/START point at an opening #\(. Return the index just past the paren that
returns depth to zero, or NIL when unbalanced."
  (let ((depth 0))
    (loop for index from start below (length value)
          for ch = (char value index)
          do (cond ((char= ch #\() (incf depth))
                   ((char= ch #\)) (decf depth)
                    (when (zerop depth) (return (1+ index))))))))

(defun %command-sub-fields-at (context value open-paren)
  "Run the command substitution whose opening #\( is at OPEN-PAREN and return
its output fields, or NIL when the parens are empty/unbalanced."
  (let ((end (%command-substitution-end value open-paren)))
    (when (and end (> end (1+ open-paren)))
      (values (%execute-command-substitution-fields
               context (subseq value (1+ open-paren) end))
              (1+ end)))))

(defun %expand-command-substitutions (context value)
  "Expand command substitutions in VALUE: fish-style (cmd) and POSIX $(cmd).
Arithmetic $((expr)) is passed through untouched so the arithmetic expander can
handle it later."
  (let ((len (length value)))
    (labels ((walk (pos parts)
               (if (>= pos len)
                   parts
                   (let ((ch (char value pos)))
                     (cond
                       ;; $(( ... )) -> leave intact for arithmetic expansion.
                       ((and (char= ch #\$) (< (+ pos 2) len)
                             (char= (char value (1+ pos)) #\()
                             (char= (char value (+ pos 2)) #\())
                        (let ((end (%paren-balanced-end value (1+ pos))))
                          (if end
                              (walk end (%append-command-substitution-string
                                         parts (subseq value pos end)))
                              (walk (1+ pos)
                                    (%append-command-substitution-char parts ch)))))
                       ;; $( ... ) POSIX command substitution.
                       ((and (char= ch #\$) (< (1+ pos) len)
                             (char= (char value (1+ pos)) #\())
                        (multiple-value-bind (fields next)
                            (%command-sub-fields-at context value (1+ pos))
                          (if next
                              (walk next (%append-command-substitution-fields parts fields))
                              (walk (1+ pos)
                                    (%append-command-substitution-char parts ch)))))
                       ;; bare ( ... ) fish-style command substitution.
                       ((char= ch #\()
                        (multiple-value-bind (fields next)
                            (%command-sub-fields-at context value pos)
                          (if next
                              (walk next (%append-command-substitution-fields parts fields))
                              (walk (1+ pos)
                                    (%append-command-substitution-char parts ch)))))
                       (t
                        (walk (1+ pos)
                              (%append-command-substitution-char parts ch))))))))
      (walk 0 (list "")))))

(defun %expand-source-arg-in-context (context arg)
  (let ((value (nshell.domain.parsing:arg-value arg))
        (environment (shell-context-environment context)))
    (case (nshell.domain.parsing:arg-quote-style arg)
      ((:single t)
       ;; Single quotes: fully literal, no expansion of any kind.
       (list value))
      (:double
       ;; Double quotes: command + variable expansion, but no globbing or
       ;; word-splitting -- always collapses to exactly one field.
       (list (apply #'concatenate 'string
                    (loop for expanded in (%expand-command-substitutions context value)
                          collect (nshell.domain.expansion:expand-double-quoted
                                   expanded environment)))))
      (t
       ;; A bare unquoted $argv expands to each argument as its own word
       ;; (fish semantics), so a function can forward its arguments verbatim.
       (if (string= value "$argv")
           (copy-list nshell.domain.expansion:*positional-args*)
           (loop for expanded in (%expand-command-substitutions context value)
                 append (nshell.domain.expansion:expand-all expanded environment)))))))

(defun %line-command-args (command-node &optional environment)
  (loop for arg in (nshell.domain.parsing:command-node-args command-node)
        append (%expand-source-arg arg environment)))

(defun %line-command-args-in-context (context command-node)
  (loop for arg in (nshell.domain.parsing:command-node-args command-node)
        append (%expand-source-arg-in-context context arg)))

(defparameter +source-definition-opening-keywords+
  '("if" "for" "while" "switch" "begin" "function"))

(defun %source-line-segments (line)
  (multiple-value-bind (tokens)
      (nshell.domain.parsing:tokenize line)
    (let ((segments nil)
          (segment-start 0))
      (loop for token in tokens
            do (when (member (nshell.domain.parsing:token-type token)
                             '(:semicolon :ampersand)
                             :test #'eq)
                 (let ((segment (string-trim '(#\Space #\Tab)
                                             (subseq line
                                                     segment-start
                                                     (nshell.domain.parsing:token-start token)))))
                   (when (plusp (length segment))
                     (push segment segments)))
                 (setf segment-start (nshell.domain.parsing:token-end token)))
            finally
              (let ((segment (string-trim '(#\Space #\Tab)
                                          (subseq line segment-start))))
                (when (plusp (length segment))
                  (push segment segments)))
              (return (nreverse segments))))))

(defun %function-start-p (line)
  (multiple-value-bind (tokens)
      (nshell.domain.parsing:tokenize line)
    (let ((words nil))
      (dolist (token tokens)
        (let ((type (nshell.domain.parsing:token-type token)))
          (when (member type '(:semicolon :ampersand :pipe :and :or)
                        :test #'eq)
            (return))
          (when (eq type :word)
            (push (nshell.domain.parsing:token-value token) words))))
      (let ((words (nreverse words)))
        (when (and (>= (length words) 2)
                   (string= (first words) "function"))
          (second words))))))

(defun %source-definition-line-depth-delta (line)
  (multiple-value-bind (tokens)
      (nshell.domain.parsing:tokenize line)
    (let ((expect-command t)
          (delta 0))
      (dolist (token tokens delta)
        (let ((type (nshell.domain.parsing:token-type token))
              (value (nshell.domain.parsing:token-value token)))
          (cond
            ((and expect-command (eq type :word))
             (when (and (stringp value)
                        (member value +source-definition-opening-keywords+
                                :test #'string=))
               (incf delta))
             (when (and (stringp value)
                        (string= value "end"))
               (decf delta))
             (setf expect-command nil))
            ((member type '(:semicolon :and :or :ampersand :pipe))
             (setf expect-command t))
            ((eq type :redirect))
            ((eq type :word)
             (setf expect-command nil))))))))

(defun %source-function-definition-consume-lines (source depth body inline-body include-inline-p)
  (loop while source
        for body-line = (pop source)
        for line-delta = (%source-definition-line-depth-delta body-line)
        do (if (and (= depth 1)
                    (= line-delta -1))
               (return (values t source depth body inline-body))
               (progn
                 (push body-line body)
                 (when include-inline-p
                   (push body-line inline-body))
                 (incf depth line-delta)))
        finally (return (values nil source depth body inline-body))))

(defun %source-function-definition-finish (context name body inline-body inline-lines remaining source-path)
  (let ((function-body (nreverse body))
        (inline-body-lines (nreverse inline-body))
        (tail (append inline-lines remaining)))
    (setf (gethash name (shell-context-function-table context))
          function-body
          (gethash name (shell-context-function-source-table context))
          source-path)
    (if inline-body-lines
        (multiple-value-bind (chunk exit-code)
            (%source-lines context inline-body-lines source-path)
          (values tail chunk exit-code))
        (values tail nil 0))))

(defun %source-function-definition (context name line lines source-path)
  (let ((body nil)
        (inline-body nil)
        (closed nil)
        (depth 1)
        (inline-lines (rest (%source-line-segments line)))
        (remaining lines))
    (multiple-value-bind (inline-closed inline-tail inline-depth new-body new-inline-body)
        (%source-function-definition-consume-lines inline-lines depth body inline-body t)
      (setf closed inline-closed
            inline-lines inline-tail
            depth inline-depth
            body new-body
            inline-body new-inline-body))
    (when (not closed)
      (multiple-value-bind (remaining-closed remaining-tail remaining-depth new-body new-inline-body)
          (%source-function-definition-consume-lines remaining depth body inline-body nil)
        (setf closed remaining-closed
              remaining remaining-tail
              depth remaining-depth
              body new-body
              inline-body new-inline-body)))
    (if closed
        (%source-function-definition-finish context name body inline-body inline-lines remaining source-path)
        (values nil (format nil "source: function ~a missing end~%" name) 2))))

(defun %source-lines (context lines &optional source-path)
  (let ((output nil)
        (code 0)
        (remaining lines))
    (loop while remaining
          for line = (pop remaining)
          for function-name = (%function-start-p line)
          do (if function-name
                 (multiple-value-bind (tail chunk exit-code)
                     (%source-function-definition context function-name line remaining source-path)
                   (setf remaining tail
                         code exit-code)
                   (when chunk (push chunk output)))
                 (multiple-value-bind (source-form tail)
                     (%collect-source-form line remaining)
                   (setf remaining tail)
                   (multiple-value-bind (chunk exit-code)
                       (%execute-source-line context source-form)
                     (when chunk (push chunk output))
                     (setf code exit-code)))))
    (values (apply #'concatenate 'string (nreverse output)) code)))
