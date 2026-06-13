;;; Reusable property-based testing helpers for nshell tests.

(in-package #:nshell/test)

(defparameter *pbt-default-trials* 100
  "Default number of generated examples checked by PBT helpers.")

(defparameter *pbt-shell-word-characters*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_./-"
  "Characters that form unquoted shell words in generated test commands.")

(defun gen-in-range (min max)
  "Return a generator for integers in the inclusive range [MIN, MAX]."
  (check-type min integer)
  (check-type max integer)
  (assert (<= min max) (min max) "MIN must be <= MAX.")
  (let ((integer-generator (gen-integer))
        (width (1+ (- max min))))
    (lambda ()
      (+ min (mod (abs (funcall integer-generator)) width)))))

(defun gen-shell-word (&key (min-length 1) (max-length 12))
  "Return a generator for shell words made of valid unquoted word characters."
  (let ((length-generator (gen-in-range min-length max-length))
        (index-generator (gen-in-range 0 (1- (length *pbt-shell-word-characters*)))))
    (lambda ()
      (let ((chars '()))
        (dotimes (i (funcall length-generator))
          (push (char *pbt-shell-word-characters* (funcall index-generator)) chars))
        (coerce (nreverse chars) 'string)))))

(defun gen-shell-command (&key (min-words 1) (max-words 4) (max-word-length 12))
  "Return a generator for simple valid shell command strings."
  (let ((word-count-generator (gen-in-range min-words max-words))
        (word-generator (gen-shell-word :max-length max-word-length)))
    (lambda ()
      (let ((words '()))
        (dotimes (i (funcall word-count-generator))
          (push (funcall word-generator) words))
        (format nil "~{~a~^ ~}" (nreverse words))))))

(defun gen-shell-pipeline (&key (min-commands 1) (max-commands 4))
  "Return a generator for pipe-separated valid shell command strings."
  (let ((command-count-generator (gen-in-range min-commands max-commands))
        (command-generator (gen-shell-command)))
    (lambda ()
      (let ((commands '()))
        (dotimes (i (funcall command-count-generator))
          (push (funcall command-generator) commands))
        (format nil "~{~a~^ | ~}" (nreverse commands))))))

(defun %pbt-binding-names (bindings)
  (mapcar #'first bindings))

(defun %pbt-binding-generators (bindings)
  (mapcar #'second bindings))

(defun %pbt-report-failure (trial bindings condition)
  (is nil "Property failed on trial ~d with counterexample ~s~@[; condition: ~a~]"
      trial bindings condition)
  nil)

(defmacro check-property ((&key (trials '*pbt-default-trials*)) bindings &body body)
  "Run BODY for TRIALS generated examples from BINDINGS.

BINDINGS has the same shape as FIVEAM:FOR-ALL bindings. BODY should return a
generalized boolean. The first failing generated binding set is reported as the
counterexample. No external shrinking library is used."
  (let ((trial (gensym "TRIAL-"))
        (values (gensym "VALUES-")))
    `(loop for ,trial from 1 to ,trials
           for ,values = (list ,@(mapcar (lambda (generator)
                                           `(funcall ,generator))
                                         (%pbt-binding-generators bindings)))
           always (destructuring-bind ,(%pbt-binding-names bindings) ,values
                    (handler-case
                        (or (progn ,@body)
                            (%pbt-report-failure
                             ,trial
                             (mapcar #'cons ',(%pbt-binding-names bindings) ,values)
                             nil))
                      (condition (condition)
                        (%pbt-report-failure
                         ,trial
                         (mapcar #'cons ',(%pbt-binding-names bindings) ,values)
                         condition)))))))

(defmacro for-all-property ((&key (trials '*pbt-default-trials*)) bindings &body body)
  "Run FIVEAM:FOR-ALL with TRIALS examples and BODY assertions."
  `(let ((*num-trials* ,trials)
         (*max-trials* ,trials))
     (for-all ,bindings
       ,@body)))
