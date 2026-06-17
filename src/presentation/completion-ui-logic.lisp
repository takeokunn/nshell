(in-package #:nshell.presentation)

(defun %candidate-text (candidate)
  (if (stringp candidate)
      candidate
      (nshell.domain.completion:candidate-text candidate)))

(defun %candidate-kind (candidate)
  (if (stringp candidate)
      :command
      (or (nshell.domain.completion:candidate-kind candidate) :command)))

(defun %candidate-description (candidate)
  (and (not (stringp candidate))
       (nshell.domain.completion:candidate-description candidate)))

(defun %common-prefix-two (left right)
  (let* ((limit (min (length left) (length right)))
         (index 0))
    (loop while (and (< index limit)
                     (char= (char left index) (char right index)))
          do (incf index))
    (subseq left 0 index)))

(defun completion-common-prefix (candidates)
  (when candidates
    (reduce #'%common-prefix-two
            (mapcar #'%candidate-text candidates))))

(defun %completion-escape-character-p (ch)
  (or (member ch '(#\Space #\Tab #\\ #\' #\" #\; #\| #\& #\(
                   #\) #\< #\> #\$ #\` #\* #\? #\[ #\] #\{
                   #\} #\! #\#)
              :test #'char=)
      (char= ch #\Newline)))

(defun %completion-double-quoted-escape-character-p (ch)
  (or (char= ch #\\)
      (char= ch #\")
      (char= ch #\$)
      (char= ch #\`)
      (char= ch #\Newline)))

(defun %completion-quote-context (input start end)
  (declare (ignore end))
  (when (and (< start (length input))
             (member (char input start) '(#\" #\') :test #'char=))
    (if (char= (char input start) #\')
        :single
        :double)))

(defun %completion-single-quoted-insertion-text (text)
  (with-output-to-string (out)
    (loop with start = 0
          for index from 0 below (length text)
          for ch = (char text index)
          do (when (char= ch #\')
               (when (< start index)
                 (write-string text out :start start :end index))
               (write-string "'\\''" out)
               (setf start (1+ index)))
          finally (when (< start (length text))
                    (write-string text out :start start)))))

(defun %completion-double-quoted-insertion-text (text)
  (with-output-to-string (out)
    (loop for ch across text
          do (when (%completion-double-quoted-escape-character-p ch)
               (write-char #\\ out))
             (write-char ch out))))

(defun %completion-insertion-text (text &key quote-context)
  (ecase quote-context
    ((nil)
     (with-output-to-string (out)
       (loop for ch across text
             do (when (%completion-escape-character-p ch)
                  (write-char #\\ out))
                (write-char ch out))))
    (:single
     (%completion-single-quoted-insertion-text text))
    (:double
     (%completion-double-quoted-insertion-text text))))

(defun %completion-unescape-token (text)
  (with-output-to-string (out)
    (let ((escaped nil))
      (loop for ch across text
            do (cond
                 (escaped
                  (write-char ch out)
                  (setf escaped nil))
                 ((char= ch #\\)
                  (setf escaped t))
                 (t
                  (write-char ch out))))
      (when escaped
        (write-char #\\ out)))))

(defun %completion-escaped-position-p (input position)
  (let ((count 0)
        (index (1- position)))
    (loop while (and (>= index 0)
                     (char= (char input index) #\\))
          do (incf count)
             (decf index))
    (oddp count)))

(defun %completion-token-separator-at-p (input position)
  (and (nshell.domain.parsing:shell-token-separator-p (char input position))
       (not (%completion-escaped-position-p input position))))

(defun %completion-token-bounds (input cursor)
  (let* ((limit (length input))
         (cursor (max 0 (min cursor limit))))
    (cond
      ((and (< cursor limit)
            (%completion-token-separator-at-p input cursor))
       (multiple-value-bind (start end found-p)
           (shell-token-range-before-position input cursor)
         (if found-p
             (values start end)
             (values cursor cursor))))
      ((and (= cursor limit)
            (plusp limit)
            (%completion-token-separator-at-p input (1- limit)))
       (values cursor cursor))
      (t
       (multiple-value-bind (start end found-p)
           (shell-token-range-at-or-after-cursor input cursor)
         (if (not found-p)
             (values cursor cursor)
             (values start end)))))))

(defun %completion-token-body-bounds (input start end)
  (let ((body-start start)
        (body-end end))
    (when (and (< body-start body-end)
               (member (char input body-start) '(#\" #\') :test #'char=))
      (incf body-start))
    (when (and (< body-start body-end)
               (member (char input (1- body-end)) '(#\" #\') :test #'char=))
      (decf body-end))
    (values body-start body-end)))

(defun maybe-extend-completion-common-prefix (state candidates)
  "Apply an unambiguous completion prefix, if CANDIDATES advance the token."
  (let* ((state (normalize-input-state state))
         (buffer (input-state-buffer state))
         (cursor (input-state-cursor-pos state))
         (prefix (completion-common-prefix candidates)))
    (if (null prefix)
        (values state nil)
        (multiple-value-bind (start end) (%completion-token-bounds buffer cursor)
          (multiple-value-bind (body-start body-end)
              (%completion-token-body-bounds buffer start end)
            (declare (ignore body-end))
            (let* ((token (subseq buffer body-start cursor))
                   (raw-token (%completion-unescape-token token)))
              (if (and (> (length prefix) (length raw-token))
                       (<= (length raw-token) (length prefix))
                       (string= raw-token (subseq prefix 0 (length raw-token))))
                  (let* ((quote-context (%completion-quote-context buffer start end))
                         (insertion (%completion-insertion-text prefix
                                                                :quote-context quote-context))
                         (quote-prefix (if quote-context
                                           (string (char buffer start))
                                           ""))
                         (new-buffer (concatenate 'string
                                                  (subseq buffer 0 start)
                                                  quote-prefix
                                                  insertion
                                                  (subseq buffer end))))
                    (values (copy-input-state-clearing-completion state
                             :buffer new-buffer
                             :cursor-pos (+ start
                                            (length quote-prefix)
                                            (length insertion)))
                            t))
                  (values state nil))))))))

(defun cycle-completion (candidates current)
  (let ((n (length candidates)))
    (if (zerop n) 0 (mod (1+ current) n))))

(defun apply-completion (input candidate &key (cursor (length input)))
  (multiple-value-bind (start end) (%completion-token-bounds input cursor)
    (let* ((quote-context (%completion-quote-context input start end))
           (text (%completion-insertion-text (%candidate-text candidate)
                                             :quote-context quote-context))
           (quote-prefix (if quote-context
                             (string (char input start))
                             "")))
        (values (concatenate 'string
                             (subseq input 0 start)
                             quote-prefix
                             text
                             (subseq input end))
                (+ start
                   (length quote-prefix)
                    (length text))))))
