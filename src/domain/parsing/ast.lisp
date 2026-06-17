;;; AST Node Types
(in-package #:nshell.domain.parsing)

(defstruct (ast-node (:constructor make-ast-node (type &optional span)))
  (type :unknown :type keyword :read-only t)
  (span nil :type list :read-only t))

(defstruct (command-node (:include ast-node)
                         (:constructor make-command-node (command args &optional span)))
  (command "" :type string :read-only t)
  (args nil :type list :read-only t))

(defstruct (pipeline-node (:include ast-node)
                          (:constructor make-pipeline-node (commands &optional span)))
  (commands nil :type list :read-only t))

(defstruct (sequence-node (:include ast-node)
                           (:constructor make-sequence-node (commands &optional separators span)))
  "Represents ;-separated sequential commands or &-separated background commands.
   SEPARATORS is a list of :semi or :amp keywords, one per command except the last."
  (commands nil :type list :read-only t)
  (separators nil :type list :read-only t))

(defstruct (if-node (:include ast-node)
                    (:constructor make-if-node (condition then-branch &optional else-branch span)))
  (condition nil :type (or null ast-node) :read-only t)
  (then-branch nil :type list :read-only t)
  (else-branch nil :type list :read-only t))

(defstruct (for-node (:include ast-node)
                     (:constructor make-for-node (var-name in-values body &optional span)))
  (var-name "" :type string :read-only t)
  (in-values nil :type list :read-only t)
  (body nil :type list :read-only t))

(defstruct (while-node (:include ast-node)
                       (:constructor make-while-node (condition body &optional span)))
  (condition nil :type (or null ast-node) :read-only t)
  (body nil :type list :read-only t))

(defstruct (case-node (:include ast-node)
                      (:constructor make-case-node (value clauses &optional span)))
  (value "" :type string :read-only t)
  (clauses nil :type list :read-only t))

(defstruct (begin-end-node (:include ast-node)
                           (:constructor make-begin-end-node (body &optional span)))
  (body nil :type list :read-only t))

(defstruct (argument-node (:include ast-node)
                          (:constructor make-argument-node (value &optional span)))
  (value "" :type string :read-only t))

(defstruct (operator-node (:include ast-node)
                          (:constructor make-operator-node (operator &optional span)))
  (operator "" :type string :read-only t))

(defstruct (error-node (:include ast-node)
                       (:constructor make-error-node (message position &optional span)))
  (message "" :type string :read-only t)
  (position 0 :type integer :read-only t))

(defstruct (incomplete-node (:include ast-node)
                            (:constructor make-incomplete-node (partial-text kind &optional span)))
  (partial-text "" :type string :read-only t)
  (kind :unknown :type keyword :read-only t))


(defun ast-complete-p (node)
  (not (or (error-node-p node) (incomplete-node-p node))))

(defun ast-has-errors-p (node)
  (labels ((check (n)
               (cond ((error-node-p n) t)
                     ((pipeline-node-p n) (some #'check (pipeline-node-commands n)))
                     ((sequence-node-p n) (some #'check (sequence-node-commands n)))
                     ((if-node-p n) (or (check (if-node-condition n))
                                        (some #'check (if-node-then-branch n))
                                        (some #'check (if-node-else-branch n))))
                     ((for-node-p n) (some #'check (for-node-body n)))
                     ((while-node-p n) (or (check (while-node-condition n))
                                           (some #'check (while-node-body n))))
                     ((case-node-p n) (some (lambda (clause)
                                              (some #'check (cdr clause)))
                                            (case-node-clauses n)))
                     ((begin-end-node-p n) (some #'check (begin-end-node-body n)))
                     (t nil))))
    (check node)))

;; -- Arg utilities (cons-based arg support) -----------------
(defun arg-value (arg)
  "Extract string value from an arg (string or (value . quoted-p) cons)."
  (if (consp arg) (car arg) arg))

(defun arg-quoted-p (arg)
  "Return T if arg was single-quoted and should not be expanded."
  (and (consp arg) (cdr arg)))

(defun command-node-arg-values (node)
  "Return all args as plain strings (unwrapping cons cells)."
  (mapcar #'arg-value (command-node-args node)))
