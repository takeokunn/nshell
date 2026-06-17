(in-package #:nshell.domain.parsing)

(defstruct (token (:constructor make-token (type value &optional (start 0) (end 0) (quoted-p nil))))
  (type :word :type keyword :read-only t)
  (value "" :type string :read-only t)
  (start 0 :type integer :read-only t)
  (end 0 :type integer :read-only t)
  (quoted-p nil :type boolean :read-only t))

;; token-type, token-value, token-start, token-end are auto-generated struct accessors

(defstruct (tokenizer-state (:constructor %make-tokenizer-state))
  input
  len
  cursor-pos
  (pos 0 :type integer)
  (tokens '() :type list)
  (incomplete nil :type boolean))

(defun shell-assignment-word-p (word)
  "Return true when WORD looks like a shell assignment word."
  (and (stringp word)
       (plusp (length word))
       (let ((equals-position (position #\= word)))
         (and equals-position
              (plusp equals-position)
              (loop for index below equals-position
                    for ch = (char word index)
                    always (if (zerop index)
                               (or (alpha-char-p ch) (char= ch #\_))
                                 (or (alphanumericp ch) (char= ch #\_))))))))

(defparameter +shell-word-separator-characters+
  '(#\Space #\Tab #\Newline)
  "Characters that separate shell words.")

(defparameter +shell-operator-separator-characters+
  '(#\| #\; #\& #\< #\>)
  "Characters that separate shell operators.")

(defparameter +shell-command-separator-token-types+
  '(:pipe :and :or :semicolon :ampersand)
  "Token types that separate shell command segments.")

(defun shell-word-separator-p (ch)
  (member ch +shell-word-separator-characters+ :test #'char=))

(defun shell-operator-separator-p (ch)
  (member ch +shell-operator-separator-characters+ :test #'char=))

(defun shell-token-separator-p (ch)
  (or (shell-word-separator-p ch)
      (shell-operator-separator-p ch)))

(defun shell-command-separator-token-p (token)
  (member (token-type token) +shell-command-separator-token-types+ :test #'eq))

(defun %shell-input-separator-p (ch include-return-p)
  (or (member ch +shell-word-separator-characters+ :test #'char=)
      (member ch +shell-operator-separator-characters+ :test #'char=)
      (and include-return-p (char= ch #\Return))))

(defun shell-input-blank-p (input &key include-return-p)
  "Return true when INPUT contains only shell separators."
  (every (lambda (ch)
           (%shell-input-separator-p ch include-return-p))
         input))

(defun make-tokenizer-state (input &key cursor-pos)
  (%make-tokenizer-state :input input
                         :len (length input)
                         :cursor-pos (or cursor-pos (length input))))

(defun %tokenizer-state-peek (state &optional (offset 0))
  (let ((p (+ (tokenizer-state-pos state) offset)))
    (if (< p (tokenizer-state-len state))
        (char (tokenizer-state-input state) p)
        nil)))

(defun %tokenizer-state-advance (state &optional (n 1))
  (incf (tokenizer-state-pos state) n))

(defun %tokenizer-state-take (state)
  (let ((ch (%tokenizer-state-peek state)))
    (%tokenizer-state-advance state)
    ch))

(defun %tokenizer-state-push-token (state type value start end &optional quoted-p)
  (push (make-token type value start end quoted-p) (tokenizer-state-tokens state)))

(defun %tokenizer-state-emit-token (state type value &optional quoted-p)
  (let ((start (tokenizer-state-pos state))
        (width (length value)))
    (%tokenizer-state-push-token state type value start (+ start width) quoted-p)
    (%tokenizer-state-advance state width)))

(defun %tokenizer-balanced-substitution-end (state start)
  (let ((depth 0)
        (quote nil)
        (escaped nil))
    (loop for index from start below (tokenizer-state-len state)
          for ch = (char (tokenizer-state-input state) index)
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

(defun %tokenizer-read-balanced-command-substitution (state)
  (let* ((start (tokenizer-state-pos state))
         (end (%tokenizer-balanced-substitution-end state start)))
    (%tokenizer-state-push-token state :word
                                 (subseq (tokenizer-state-input state) start (1+ end))
                                 start
                                 (1+ end))
    (setf (tokenizer-state-pos state) (1+ end))))

(defun %tokenizer-read-balanced-process-substitution (state)
  (let ((start (tokenizer-state-pos state))
        (chars '())
        (depth 0)
        (quote-delimiter nil)
        (escaped nil))
    (push (%tokenizer-state-take state) chars)
    (push (%tokenizer-state-take state) chars)
    (setf depth 1)
    (loop while (and (< (tokenizer-state-pos state) (tokenizer-state-len state))
                     (plusp depth))
          for ch = (%tokenizer-state-take state)
          do (progn
               (push ch chars)
               (cond
                 (escaped
                  (setf escaped nil))
                 ((char= ch #\\)
                  (setf escaped t))
                 (quote-delimiter
                  (when (char= ch quote-delimiter)
                    (setf quote-delimiter nil)))
                 ((or (char= ch #\') (char= ch #\"))
                  (setf quote-delimiter ch))
                 ((char= ch #\() (incf depth))
                 ((char= ch #\)) (decf depth)))))
    (let ((value (coerce (nreverse chars) 'string)))
      (if (plusp depth)
          (progn
            (setf (tokenizer-state-incomplete state) t)
            (%tokenizer-state-push-token state :error value start (tokenizer-state-pos state)))
          (%tokenizer-state-push-token state :word value start (tokenizer-state-pos state))))))

(defun %tokenizer-read-word (state)
  (let ((start (tokenizer-state-pos state))
        (chars '()))
    (loop while (< (tokenizer-state-pos state) (tokenizer-state-len state))
          for ch = (%tokenizer-state-peek state)
          do (cond
               ((or (char= ch #\Space) (char= ch #\Tab)
                    (char= ch #\Newline)
                    (char= ch #\|) (char= ch #\>)
                    (char= ch #\<) (char= ch #\&)
                    (char= ch #\;) (char= ch #\()
                    (char= ch #\)) (char= ch #\')
                    (char= ch #\"))
                (return))
               ((char= ch #\\)
                (let ((escape-start (tokenizer-state-pos state)))
                  (%tokenizer-state-advance state)
                  (if (< (tokenizer-state-pos state) (tokenizer-state-len state))
                      (progn
                        (push (%tokenizer-state-peek state) chars)
                        (%tokenizer-state-advance state))
                      (progn
                        (setf (tokenizer-state-incomplete state) t)
                        (when chars
                          (%tokenizer-state-push-token state :word
                                                       (coerce (nreverse chars) 'string)
                                                       start
                                                       escape-start))
                        (%tokenizer-state-push-token state :error "\\" escape-start
                                                     (tokenizer-state-pos state))
                        (return)))))
               (t
                (push ch chars)
                (%tokenizer-state-advance state))))
    (when chars
      (%tokenizer-state-push-token state :word
                                   (coerce (nreverse chars) 'string)
                                   start
                                   (tokenizer-state-pos state)))))

(defun %tokenizer-read-delimited (state delimiter &key escape-p quoted-p)
  (let ((start (tokenizer-state-pos state))
        (chars '()))
    (%tokenizer-state-advance state)
    (loop while (< (tokenizer-state-pos state) (tokenizer-state-len state))
          for ch = (%tokenizer-state-peek state)
          do (cond
               ((char= ch delimiter)
                (return))
               ((and escape-p (char= ch #\\))
                (%tokenizer-state-advance state)
                (when (< (tokenizer-state-pos state) (tokenizer-state-len state))
                  (push (%tokenizer-state-peek state) chars)
                  (%tokenizer-state-advance state)))
               (t
                (push ch chars)
                (%tokenizer-state-advance state))))
    (if (and (< (tokenizer-state-pos state) (tokenizer-state-len state))
             (char= (%tokenizer-state-peek state) delimiter))
        (progn
          (%tokenizer-state-advance state)
          (%tokenizer-state-push-token state :word (coerce (nreverse chars) 'string)
                                       start (tokenizer-state-pos state)
                                       quoted-p))
        (progn
          (setf (tokenizer-state-incomplete state) t)
          (%tokenizer-state-push-token state :error (coerce (nreverse chars) 'string) start (tokenizer-state-pos state))))))

(defun %tokenizer-read-single-quoted (state)
  (%tokenizer-read-delimited state #\' :quoted-p t))

(defun %tokenizer-read-double-quoted (state)
  (%tokenizer-read-delimited state #\" :escape-p t))

(defun %tokenizer-read-escaped (state)
  (let ((start (tokenizer-state-pos state)))
    (%tokenizer-state-advance state)
    (if (< (tokenizer-state-pos state) (tokenizer-state-len state))
        (progn
          (%tokenizer-state-push-token state :word (string (%tokenizer-state-peek state)) start (1+ (tokenizer-state-pos state)))
          (%tokenizer-state-advance state))
        (progn
          (setf (tokenizer-state-incomplete state) t)
          (%tokenizer-state-push-token state :error "\\" start (tokenizer-state-pos state))))))

(defun %tokenizer-read-comment (state)
  (loop while (< (tokenizer-state-pos state) (tokenizer-state-len state))
        for ch = (%tokenizer-state-peek state)
        until (char= ch #\Newline)
        do (%tokenizer-state-advance state)))

(defun tokenize-into-state (state)
  (loop while (< (tokenizer-state-pos state) (tokenizer-state-len state))
        do (let ((ch (%tokenizer-state-peek state)))
             (cond ((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline))
                    (%tokenizer-state-advance state))
                   ((char= ch #\&)
                    (if (and (%tokenizer-state-peek state 1)
                             (char= (%tokenizer-state-peek state 1) #\&))
                        (%tokenizer-state-emit-token state :and "&&")
                        (%tokenizer-state-emit-token state :ampersand "&")))
                   ((char= ch #\|)
                    (if (and (%tokenizer-state-peek state 1)
                             (char= (%tokenizer-state-peek state 1) #\|))
                        (%tokenizer-state-emit-token state :or "||")
                        (%tokenizer-state-emit-token state :pipe "|")))
                   ((and (char= ch #\>) (%tokenizer-state-peek state 1) (char= (%tokenizer-state-peek state 1) #\>))
                    (%tokenizer-state-emit-token state :redirect ">>"))
                   ((char= ch #\>) (%tokenizer-state-emit-token state :redirect ">"))
                   ((and (char= ch #\<) (%tokenizer-state-peek state 1) (char= (%tokenizer-state-peek state 1) #\())
                    (%tokenizer-read-balanced-process-substitution state))
                   ((char= ch #\<) (%tokenizer-state-emit-token state :redirect "<"))
                   ((char= ch #\;) (%tokenizer-state-emit-token state :semicolon ";"))
                   ((and (char= ch #\()
                         (%tokenizer-state-peek state 1)
                         (char/= (%tokenizer-state-peek state 1) #\))
                         (%tokenizer-balanced-substitution-end state (tokenizer-state-pos state)))
                    (%tokenizer-read-balanced-command-substitution state))
                   ((char= ch #\() (%tokenizer-state-emit-token state :lparen "("))
                   ((char= ch #\)) (%tokenizer-state-emit-token state :rparen ")"))
                   ((char= ch #\#) (%tokenizer-read-comment state))
                   ((char= ch #\') (%tokenizer-read-single-quoted state))
                   ((char= ch #\") (%tokenizer-read-double-quoted state))
                   (t (%tokenizer-read-word state)))))
  (let* ((ordered-tokens (nreverse (tokenizer-state-tokens state)))
         (cursor-tok (find-if (lambda (tok)
                                (and (>= (or (tokenizer-state-cursor-pos state) (tokenizer-state-len state))
                                         (token-start tok))
                                     (< (or (tokenizer-state-cursor-pos state) (tokenizer-state-len state))
                                        (token-end tok))))
                              ordered-tokens)))
    (values ordered-tokens cursor-tok (tokenizer-state-incomplete state))))

(defun tokenize (input &key (cursor-pos nil))
  (tokenize-into-state (make-tokenizer-state input :cursor-pos cursor-pos)))
