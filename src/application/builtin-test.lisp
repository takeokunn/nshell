(in-package #:nshell.application)

(defmacro define-test-predicate-table (name &body specs)
  `(defparameter ,name
     (list ,@(mapcar (lambda (spec)
                       (destructuring-bind (operator lambda-list &body body) spec
                         `(cons ,operator (lambda ,lambda-list ,@body))))
                     specs))))

(define-test-predicate-table +test-unary-predicates+
  ("-f" (context operand) (%path-file-p context operand))
  ("-d" (context operand) (%path-directory-p context operand))
  ("-n" (context operand) (declare (ignore context)) (not (string= operand "")))
  ("-z" (context operand) (declare (ignore context)) (string= operand "")))

(define-test-predicate-table +test-binary-predicates+
  ("=" (context left right) (declare (ignore context)) (string= left right))
  ("!=" (context left right) (declare (ignore context)) (not (string= left right))))

(defun %lookup-test-predicate (operator predicates)
  (cdr (assoc operator predicates :test #'string=)))

(defun %test-unary-predicate-p (context op operand)
  (let ((predicate (%lookup-test-predicate op +test-unary-predicates+)))
    (and predicate (funcall predicate context operand))))

(defun %test-binary-predicate-p (context left op right)
  (let ((predicate (%lookup-test-predicate op +test-binary-predicates+)))
    (and predicate (funcall predicate context left right))))

(defun %test-truthy-p (context args)
  (case (length args)
    (0 nil)
    (1 (not (string= (first args) "")))
    (2 (%test-unary-predicate-p context (first args) (second args)))
    (3 (%test-binary-predicate-p context (first args) (second args) (third args)))
    (otherwise nil)))

(defun %builtin-test (context args)
  (if (%test-truthy-p context args)
      (values nil 0)
      (values nil 1)))

(defun %builtin-bracket (context args)
  (if (and args (string= (car (last args)) "]"))
      (%builtin-test context (butlast args))
      (values (format nil "[: missing ]~%") 2)))
