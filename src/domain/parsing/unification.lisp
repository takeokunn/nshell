(in-package #:nshell.domain.parsing)

(defstruct (logic-var (:constructor make-var (name)))
  (name nil :type (or string symbol) :read-only t))

(defun var-p (x) (logic-var-p x))

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

;; Sentinel for unification failure
(defvar *unify-fail* (cons :fail :fail))

(defun unify (x y &optional (bindings '()))
  "Unify X and Y under BINDINGS. Returns bindings on success, *UNIFY-FAIL* on failure.
Use UNIFY-P to check success."
  (let ((x1 (if (var-p x) (lookup-var x bindings) x))
        (y1 (if (var-p y) (lookup-var y bindings) y)))
    (cond
      ((and (var-p x1) (var-p y1) (eq x1 y1)) bindings)
      ((var-p x1) (if (occurs-check x1 y1 bindings) *unify-fail* (extend-bindings x1 y1 bindings)))
      ((var-p y1) (if (occurs-check y1 x1 bindings) *unify-fail* (extend-bindings y1 x1 bindings)))
      ((and (atom x1) (atom y1)) (if (equal x1 y1) bindings *unify-fail*))
      ((and (consp x1) (consp y1))
       (let ((b (unify (car x1) (car y1) bindings)))
         (if (eq b *unify-fail*) *unify-fail* (unify (cdr x1) (cdr y1) b))))
      (t *unify-fail*))))

(defun unify-p (result)
  "True if unification succeeded (not *UNIFY-FAIL*)."
  (not (eq result *unify-fail*)))

(defun backtrack (goals &optional (bindings '()))
  (if (null goals) bindings
    (let ((goal (car goals)) (rest (cdr goals)))
      (let ((result (funcall goal bindings)))
        (if (and result (not (eq result *unify-fail*)))
            (let ((final (backtrack rest result)))
              (if final final nil))
            nil)))))

(defun walk (term bindings)
  (let ((resolved (if (var-p term) (lookup-var term bindings) term)))
    (cond ((var-p resolved) resolved)
          ((consp resolved) (cons (walk (car resolved) bindings) (walk (cdr resolved) bindings)))
          (t resolved))))
