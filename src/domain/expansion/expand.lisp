;;; Shell expansion engine
(in-package #:nshell.domain.expansion)

(defun pathname-directory-string (path)
  (let ((dir (pathname-directory (pathname path))))
    (cond
      ((and dir (member :absolute dir))
       (format nil "/~{~a/~}" (remove :absolute dir)))
      ((and dir (member :relative dir))
       (format nil "~{~a/~}" (remove :relative dir)))
      (t ""))))

(defun glob-root (pattern)
  (let ((wild (position-if (lambda (ch)
                             (member ch '(#\* #\? #\[) :test #'char=))
                           pattern)))
    (if wild
        (let* ((prefix (subseq pattern 0 wild))
               (slash (position #\/ prefix :from-end t)))
          (if slash
              (subseq prefix 0 (1+ slash))
              "./"))
        (pathname-directory-string pattern))))

;; Dynamic variable for filesystem operations (DDD: domain should not call uiop directly)
(defvar *glob-directory-files-fn* nil
  "Function to list files in a directory. Set to (lambda (dir) (uiop:directory-files dir)) by infrastructure.
   If NIL, glob expansion always returns the pattern unchanged.")

(defvar *glob-subdirectories-fn* nil
  "Function to list subdirectories. Set by infrastructure layer.")

(defun %recursive-directory-files-visit (dir files)
  (dolist (file (funcall *glob-directory-files-fn* dir) files)
    (push file files))
  (dolist (subdir (funcall *glob-subdirectories-fn* dir) files)
    (setf files (%recursive-directory-files-visit subdir files))))

(defun recursive-directory-files (root)
  (unless *glob-directory-files-fn* (return-from recursive-directory-files nil))
  (handler-case (%recursive-directory-files-visit (pathname root) nil)
    (error () nil)))

(defun immediate-directory-files (root)
  (unless *glob-directory-files-fn* (return-from immediate-directory-files nil))
  (handler-case (funcall *glob-directory-files-fn* (pathname root))
    (error () nil)))

(defun enough-path (file root)
  (namestring (enough-namestring file (pathname root))))

;;; Shell glob expansion helpers

(defun glob-char-p (ch)
  (member ch '(#\* #\? #\[) :test #'char=))

(defun glob-pattern-p (pattern)
  (some #'glob-char-p pattern))

(defun bracket-negation-p (pattern start end)
  (and (< start end)
       (member (char pattern start) '(#\! #\^) :test #'char=)))

(defun bracket-range-member-p (pattern start end ch)
  (loop with index = start
        while (< index end)
        thereis (let ((left (char pattern index)))
                  (cond
                    ((and (< (+ index 2) end)
                          (char= (char pattern (1+ index)) #\-))
                     (let ((right (char pattern (+ index 2))))
                       (incf index 3)
                       (char<= left ch right)))
                    (t
                     (incf index)
                     (char= left ch))))))

(defun bracket-match-p (pattern-index pattern ch)
  (let ((end (position #\] pattern :start (1+ pattern-index))))
    (if end
        (let* ((content-start (1+ pattern-index))
               (negated-p (bracket-negation-p pattern content-start end))
               (match-start (if negated-p (1+ content-start) content-start))
               (matched-p (bracket-range-member-p pattern match-start end ch)))
          (values (if negated-p (not matched-p) matched-p)
                  (1+ end)
                  t))
        (values nil (1+ pattern-index) nil))))

(defun %glob-match-p-at (pattern pattern-length text text-length pidx tidx)
  (cond
    ((= pidx pattern-length) (= tidx text-length))
    ((and (< (1+ pidx) pattern-length)
          (char= (char pattern pidx) #\*)
          (char= (char pattern (1+ pidx)) #\*))
     (or (%glob-match-p-at pattern pattern-length text text-length (+ pidx 2) tidx)
         (and (< tidx text-length)
              (%glob-match-p-at pattern pattern-length text text-length pidx
                                (1+ tidx)))))
    ((char= (char pattern pidx) #\*)
     (or (%glob-match-p-at pattern pattern-length text text-length (1+ pidx) tidx)
         (and (< tidx text-length)
              (char/= (char text tidx) #\/)
              (%glob-match-p-at pattern pattern-length text text-length pidx
                                (1+ tidx)))))
    ((char= (char pattern pidx) #\?)
     (and (< tidx text-length)
          (char/= (char text tidx) #\/)
          (%glob-match-p-at pattern pattern-length text text-length (1+ pidx)
                            (1+ tidx))))
    ((char= (char pattern pidx) #\[)
     (and (< tidx text-length)
          (multiple-value-bind (ok next-pidx parsed-p)
              (bracket-match-p pidx pattern (char text tidx))
            (if parsed-p
                (and ok (%glob-match-p-at pattern pattern-length text text-length
                                          next-pidx (1+ tidx)))
                (and (char= (char text tidx) #\[)
                     (%glob-match-p-at pattern pattern-length text text-length
                                       (1+ pidx) (1+ tidx)))))))
    (t (and (< tidx text-length)
            (char= (char pattern pidx) (char text tidx))
            (%glob-match-p-at pattern pattern-length text text-length (1+ pidx)
                              (1+ tidx))))))

(defun glob-match-p (pattern text)
  "Return true when TEXT matches shell-style PATTERN."
  (%glob-match-p-at pattern (length pattern) text (length text) 0 0))

(defun variable-name-char-p (ch)
  (or (alphanumericp ch) (char= ch #\_)))

(defun variable-name-start-p (ch)
  (or (alpha-char-p ch) (char= ch #\_)))

(defun %parameter-name-end (content)
  "Return the index in CONTENT where the parameter name ends (i.e. the start of
an operator such as :-, :=, :+, :?), or the length of CONTENT when it is a plain
name."
  (or (position-if-not #'variable-name-char-p content)
      (length content)))

(defun %string-replace (value old new global)
  "Replace OLD (a literal substring) with NEW in VALUE, the first occurrence only
unless GLOBAL is true."
  (if (zerop (length old))
      value
      (with-output-to-string (out)
        (loop with start = 0
              for pos = (search old value :start2 start)
              while pos
              do (write-string (subseq value start pos) out)
                 (write-string new out)
                 (setf start (+ pos (length old)))
                 (unless global
                   (write-string (subseq value start) out)
                   (return))
              finally (write-string (subseq value start) out)))))

(defun %param-strip-prefix (value rest env)
  "Strip a glob-matching prefix from VALUE. REST is the text after the first #;
a further leading # selects the longest match instead of the shortest."
  (let* ((longest (and (plusp (length rest)) (char= (char rest 0) #\#)))
         (pattern (expand-variables (subseq rest (if longest 1 0)) env)))
    (if (zerop (length pattern))
        value
        (dolist (i (if longest
                       (loop for i from (length value) downto 0 collect i)
                       (loop for i from 0 to (length value) collect i))
                   value)
          (when (glob-match-p pattern (subseq value 0 i))
            (return (subseq value i)))))))

(defun %param-strip-suffix (value rest env)
  "Strip a glob-matching suffix from VALUE. REST is the text after the first %;
a further leading % selects the longest match instead of the shortest."
  (let* ((longest (and (plusp (length rest)) (char= (char rest 0) #\%)))
         (pattern (expand-variables (subseq rest (if longest 1 0)) env))
         (len (length value)))
    (if (zerop (length pattern))
        value
        (dolist (i (if longest
                       (loop for i from 0 to len collect i)
                       (loop for i from len downto 0 collect i))
                   value)
          (when (glob-match-p pattern (subseq value i))
            (return (subseq value 0 i)))))))

(defun %param-substitute (value rest env)
  "Substitute within VALUE for ${VAR/pat/rep}. REST is the text after the first
/; a further leading / replaces all occurrences. PAT is matched literally."
  (let* ((global (and (plusp (length rest)) (char= (char rest 0) #\/)))
         (body (subseq rest (if global 1 0)))
         (slash (position #\/ body))
         (pat (expand-variables (if slash (subseq body 0 slash) body) env))
         (rep (expand-variables (if slash (subseq body (1+ slash)) "") env)))
    (%string-replace value pat rep global)))

(defun %expand-braced-parameter (content env)
  "Expand the CONTENT of a ${...} parameter expansion.
Supports plain ${NAME}, length ${#NAME}, the POSIX default/alternate operators
${NAME:-word} / :- / := / :+ / :? (a leading colon makes the test fire on unset
OR empty), prefix/suffix stripping ${NAME#pat} / ## / % / %% (glob patterns),
and substitution ${NAME/pat/rep} / // (literal patterns). WORD/patterns are
themselves variable-expanded. The := assignment side effect is not performed."
  (cond
    ;; ${#NAME} -> length (only when # precedes a bare name, not ${NAME#pat}).
    ((and (plusp (length content)) (char= (char content 0) #\#)
          (every #'variable-name-char-p (subseq content 1)))
     (let ((value (nshell.domain.environment:env-get env (subseq content 1))))
       (princ-to-string (length (or value "")))))
    (t
     (let* ((op-pos (%parameter-name-end content))
            (name (subseq content 0 op-pos))
            (rest (subseq content op-pos))
            (raw (nshell.domain.environment:env-get env name))
            (set-p (not (null raw)))
            (value (or raw "")))
       (if (zerop (length rest))
           value
           (let* ((colon (char= (char rest 0) #\:))
                  (op-index (if colon 1 0))
                  (op (when (< op-index (length rest)) (char rest op-index)))
                  (word (expand-variables
                         (subseq rest (min (length rest) (1+ op-index))) env))
                  (fire (if colon
                            (or (not set-p) (zerop (length value)))
                            (not set-p))))
             (case op
               ((#\- #\=) (if fire word value))
               (#\+ (if fire "" word))
               (#\? (if fire word value))
               (#\# (%param-strip-prefix value (subseq rest 1) env))
               (#\% (%param-strip-suffix value (subseq rest 1) env))
               (#\/ (%param-substitute value (subseq rest 1) env))
               (t (concatenate 'string value rest)))))))))

(defun expand-variables (input env)
  "Expand $VAR and ${VAR} occurrences in INPUT using ENV.
Undefined variables expand to the empty string."
  (with-output-to-string (out)
    (loop with len = (length input)
          for i from 0 below len
          for ch = (char input i)
          do (cond
               ((char/= ch #\$) (write-char ch out))
               ((>= (1+ i) len) (write-char ch out))
               ((char= (char input (1+ i)) #\{)
                (let ((end (position #\} input :start (+ i 2))))
                  (if end
                      (progn
                        (write-string
                         (%expand-braced-parameter (subseq input (+ i 2) end) env)
                         out)
                        (setf i end))
                      (write-char ch out))))
               ((variable-name-start-p (char input (1+ i)))
                (let ((start (1+ i))
                      (end (loop for j from (1+ i) below len
                                 while (variable-name-char-p (char input j))
                                 finally (return j))))
                  (write-string (or (nshell.domain.environment:env-get env (subseq input start end)) "") out)
                  (setf i (1- end))))
               (t (write-char ch out))))))

(defun starts-with-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun expand-tilde (input env)
  "Expand leading ~ to HOME and ~USER to /home/USER."
  (cond
    ((string= input "~") (or (nshell.domain.environment:env-get env "HOME") "~"))
    ((starts-with-p "~/" input)
     (concatenate 'string (or (nshell.domain.environment:env-get env "HOME") "~")
                  (subseq input 1)))
    ((and (> (length input) 1) (char= (char input 0) #\~))
     (let ((slash (position #\/ input)))
       (if slash
           (concatenate 'string "/home/" (subseq input 1 slash) (subseq input slash))
           (concatenate 'string "/home/" (subseq input 1)))))
    (t input)))

(defun expand-glob (pattern)
  "Expand PATTERN containing *, ?, [abc], or ** into matching path strings.
Returns a one-element list containing PATTERN when it has no glob syntax or no matches."
  (if (not (glob-pattern-p pattern))
      (list pattern)
      (let* ((root (glob-root pattern))
             (recursive-p (search "**" pattern))
             (files (if recursive-p
                        (recursive-directory-files root)
                        (immediate-directory-files root)))
             (matches (remove-if-not
                       (lambda (file)
                         (let* ((relative (enough-path file root))
                                (candidate (if (string= root "./")
                                               relative
                                               (concatenate 'string root relative))))
                           (glob-match-p pattern candidate)))
                       files)))
        (if matches
            (sort (mapcar #'namestring matches) #'string<)
            (list pattern)))))

(defun %find-matching-brace (string start)
  "Return the index of the #\} matching the #\{ at START, or NIL if unbalanced."
  (loop with depth = 0
        for i from start below (length string)
        for ch = (char string i)
        do (cond ((char= ch #\{) (incf depth))
                 ((char= ch #\}) (decf depth) (when (zerop depth) (return i))))))

(defun %split-top-level-commas (string)
  "Split STRING on commas that are not nested inside braces."
  (let ((parts '()) (depth 0) (start 0))
    (loop for i from 0 below (length string)
          for ch = (char string i)
          do (cond ((char= ch #\{) (incf depth))
                   ((char= ch #\}) (decf depth))
                   ((and (char= ch #\,) (zerop depth))
                    (push (subseq string start i) parts)
                    (setf start (1+ i)))))
    (push (subseq string start) parts)
    (nreverse parts)))

(defun %brace-range-expansion (content)
  "Return the list of expansions for a numeric (1..5) or single-character (a..e)
range CONTENT, or NIL when CONTENT is not a valid range."
  (let ((dots (search ".." content)))
    (when dots
      (let ((lo (subseq content 0 dots))
            (hi (subseq content (+ dots 2))))
        (cond
          ((and (plusp (length lo)) (every #'digit-char-p lo)
                (plusp (length hi)) (every #'digit-char-p hi))
           (let ((a (parse-integer lo)) (b (parse-integer hi)))
             (if (<= a b)
                 (loop for n from a to b collect (princ-to-string n))
                 (loop for n from a downto b collect (princ-to-string n)))))
          ((and (= 1 (length lo)) (alpha-char-p (char lo 0))
                (= 1 (length hi)) (alpha-char-p (char hi 0)))
           (let ((a (char-code (char lo 0))) (b (char-code (char hi 0))))
             (if (<= a b)
                 (loop for c from a to b collect (string (code-char c)))
                 (loop for c from a downto b collect (string (code-char c))))))
          (t nil))))))

(defun expand-braces (input)
  "Expand brace patterns {a,b,c} and ranges {1..5}/{a..e} in INPUT, returning a
list of strings (always at least one). A brace group with no top-level comma and
no valid range is left literal, matching shell behavior."
  (let ((open (position #\{ input)))
    (if (null open)
        (list input)
        (let ((close (%find-matching-brace input open)))
          (if (null close)
              (list input)
              (let* ((prefix (subseq input 0 open))
                     (content (subseq input (1+ open) close))
                     (suffix (subseq input (1+ close)))
                     (options (or (%brace-range-expansion content)
                                  (let ((parts (%split-top-level-commas content)))
                                    (when (> (length parts) 1) parts)))))
                (if (null options)
                    (mapcar (lambda (s)
                              (concatenate 'string prefix "{" content "}" s))
                            (expand-braces suffix))
                    (loop for opt in options
                          append (loop for opt-exp in (expand-braces opt)
                                       append (loop for suf in (expand-braces suffix)
                                                    collect (concatenate 'string
                                                                         prefix opt-exp suf)))))))))))

(defun expand-all (input env)
  "Apply brace, tilde, arithmetic, variable, and glob expansion to INPUT,
returning the (possibly multiple) resulting fields."
  (loop for braced in (expand-braces input)
        append (expand-glob
                (expand-variables
                 (expand-arithmetic (expand-tilde braced env) env) env))))

(defun expand-double-quoted (input env)
  "Expand INPUT as the contents of a double-quoted string.
Variables are expanded, but tilde, globbing, and word-splitting are suppressed
\(POSIX semantics), so the result is always a single string."
  (expand-variables input env))
