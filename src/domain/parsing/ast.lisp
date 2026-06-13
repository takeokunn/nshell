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

(defun ast-node-type (node) (ast-node-type-slot node))

(defun ast-complete-p (node)
  (not (or (error-node-p node) (incomplete-node-p node))))

(defun ast-has-errors-p (node)
  (labels ((check (n)
             (cond ((error-node-p n) t)
                   ((pipeline-node-p n) (some #'check (pipeline-node-commands n)))
                   (t nil))))
    (check node)))
