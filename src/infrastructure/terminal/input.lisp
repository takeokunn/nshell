(in-package #:nshell.infrastructure.terminal)

(defstruct (key-event (:constructor make-key-event (type &optional char number)))
  "Decoded terminal input event.

TYPE is a keyword such as :CHAR, :ENTER, :TAB, :LEFT, :CTRL-C, or
:SHIFT-TAB. CHAR is populated for printable character events. NUMBER is an
optional numeric payload for future terminal protocols."
  (type :char :type keyword :read-only t)
  (char nil :type (or null character) :read-only t)
  (number nil :type (or null integer) :read-only t))

(defconstant +escape+ #.(code-char 27))

(defun control-char-p (ch code)
  (= (char-code ch) code))

(defun printable-char-p (ch)
  (<= 32 (char-code ch) 126))

(defun read-available-char (&key (attempts 20) (sleep-seconds 0.001))
  (loop repeat attempts
        when (listen *standard-input*)
          return (read-char *standard-input* nil nil)
        do (sleep sleep-seconds)
        finally (return nil)))

(defun read-available-chars (&key (limit 32))
  "Read immediately available characters after an ESC byte.

This keeps normal ESC usable while decoding common CSI/SS3 sequences. It does
not wait for more bytes if the terminal has not delivered them yet."
  (let ((chars '()))
    (loop for ch = (read-available-char)
          while (and ch (< (length chars) limit))
          do (push ch chars)
          while (listen *standard-input*))
    (coerce (remove nil (nreverse chars)) 'string)))

(defun split-string-on-char (string delimiter)
  (let ((parts '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          do (if pos
                 (progn
                   (push (subseq string start pos) parts)
                   (setf start (1+ pos)))
                 (progn
                   (push (subseq string start) parts)
                   (return))))
    (nreverse parts)))

(defun parse-integer-or-nil (string)
  (when (> (length string) 0)
    (handler-case (parse-integer string)
      (error () nil))))

(defun csi-mouse-sequence-p (body final)
  (or (char= final #\M)
      (and (> (length body) 0)
           (or (char= (char body 0) #\<)
               (char= (char body 0) #\M)))))

(defun csi-modifier (body)
  (let* ((parts (split-string-on-char body #\;))
         (modifier-text (second parts)))
    (parse-integer-or-nil (or modifier-text ""))))

(defun modified-arrow-type (base modifier)
  (case modifier
    (2 (case base
         (:up :shift-up)
         (:down :shift-down)
         (:right :shift-right)
         (:left :shift-left)
         (otherwise base)))
    (otherwise base)))

(defun decode-csi-final (body final)
  (when (csi-mouse-sequence-p body final)
    (return-from decode-csi-final (make-key-event :ignore)))
  (case final
    (#\A (make-key-event (modified-arrow-type :up (csi-modifier body))))
    (#\B (make-key-event (modified-arrow-type :down (csi-modifier body))))
    (#\C (make-key-event (modified-arrow-type :right (csi-modifier body))))
    (#\D (make-key-event (modified-arrow-type :left (csi-modifier body))))
    (#\H (make-key-event :home))
    (#\F (make-key-event :end))
    (#\Z (make-key-event :shift-tab))
    (#\~ (let ((code (parse-integer-or-nil (first (split-string-on-char body #\;)))))
           (case code
             (1 (make-key-event :home))
             (3 (make-key-event :delete))
             (4 (make-key-event :end))
             (7 (make-key-event :home))
             (8 (make-key-event :end))
             (otherwise (make-key-event :unknown nil code)))))
    (otherwise (make-key-event :unknown))))

(defun decode-csi-sequence (sequence)
  (if (zerop (length sequence))
      (make-key-event :escape)
      (let* ((final (char sequence (1- (length sequence))))
             (body (subseq sequence 0 (1- (length sequence)))))
        (decode-csi-final body final))))

(defun decode-ss3-sequence (sequence)
  (if (zerop (length sequence))
      (make-key-event :escape)
      (case (char sequence 0)
        (#\A (make-key-event :up))
        (#\B (make-key-event :down))
        (#\C (make-key-event :right))
        (#\D (make-key-event :left))
        (#\H (make-key-event :home))
        (#\F (make-key-event :end))
        (otherwise (make-key-event :unknown)))))

(defun decode-escape-sequence (sequence)
  (cond
    ((zerop (length sequence)) (make-key-event :escape))
    ((char= (char sequence 0) #\[)
     (decode-csi-sequence (subseq sequence 1)))
    ((char= (char sequence 0) #\O)
     (decode-ss3-sequence (subseq sequence 1)))
    (t (make-key-event :escape))))

(defun decode-control-key (ch)
  (case (char-code ch)
    (1 (make-key-event :ctrl-a))
    (2 (make-key-event :ctrl-b))
    (3 (make-key-event :ctrl-c))
    (4 (make-key-event :ctrl-d))
    (5 (make-key-event :ctrl-e))
    (6 (make-key-event :ctrl-f))
    (9 (make-key-event :tab))
    (10 (make-key-event :enter))
    (11 (make-key-event :ctrl-k))
    (13 (make-key-event :enter))
    (18 (make-key-event :ctrl-r))
    (21 (make-key-event :ctrl-u))
    (23 (make-key-event :ctrl-w))
    (127 (make-key-event :backspace))
    (otherwise nil)))

(defun decode-character-key (ch)
  (cond
    ((char= ch +escape+)
     (decode-escape-sequence (read-available-chars)))
    ((decode-control-key ch))
    ((printable-char-p ch) (make-key-event :char ch))
    (t (make-key-event :unknown ch))))

(defun read-key-event ()
  "Read and decode one terminal key event from `*standard-input*'."
  (let ((ch (read-char *standard-input* nil nil)))
    (when ch
      (decode-character-key ch))))

;; key-event-type, key-event-char, and key-event-number are struct accessors.
