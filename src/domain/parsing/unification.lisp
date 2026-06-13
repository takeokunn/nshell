(in-package #:nshell.domain.parsing)

(defstruct (logic-var (:constructor make-var (name)))
  (name nil :type (or string symbol) :read-only t))

(defun var-p (x) (logic-var-p x))
(defun var-name (v) (logic-var-name v))

(defun empty-bindings () '())

(defun lookup-var (var bindings)
  (let ((pair (assoc var bindings :test #'eq)))
    (if pair
        (let ((val (cdr pair)))
          (if (var-p val) (lookup-var val bindings) val))
        var)))

(defun extend-bindings (var value bindings)
  (acons var value bindings))

(defun occurs-check (var term bindings)
  (cond
    ((eq var term) t)
    ((var-p term)
     (let ((val (lookup-var term bindings)))
       (if (eq val term) nil (occurs-check var val bindings))))
    ((consp term)
     (or (occurs-check var (car term) bindings)
         (occurs-check var (cdr term) bindings)))
    (t nil)))

(defun unify (x y &optional (bindings '()))
  (let ((x1 (if (var-p x) (lookup-var x bindings) x))
        (y1 (if (var-p y) (lookup-var y bindings) y)))
    (cond
      ((and (var-p x1) (var-p y1) (eq x1 y1)) bindings)
      ((var-p x1) (if (occurs-check x1 y1 bindings) nil (extend-bindings x1 y1 bindings)))
      ((var-p y1) (if (occurs-check y1 x1 bindings) nil (extend-bindings y1 x1 bindings)))
      ((and (atom x1) (atom y1)) (if (equal x1 y1) bindings nil))
      ((and (consp x1) (consp y1))
       (let ((b (unify (car x1) (car y1) bindings)))
         (when b (unify (cdr x1) (cdr y1) b))))
      (t nil))))

(defun backtrack (goals &optional (bindings '()))
  (if (null goals)
      bindings
      (let ((goal (car goals)) (rest (cdr goals)))
        (labels ((try (b)
                   (when b
                     (let ((result (funcall goal b)))
                       (when result
                         (let ((final (backtrack rest result)))
                           (when final (return-from backtrack final))))))))
          (try bindings)
          nil))))

(defun walk (term bindings)
  (let ((resolved (if (var-p term) (lookup-var term bindings) term)))
    (cond ((var-p resolved) resolved)
          ((consp resolved) (cons (walk (car resolved) bindings) (walk (cdr resolved) bindings)))
          (t resolved))))
