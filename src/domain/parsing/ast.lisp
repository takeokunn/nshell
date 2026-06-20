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
  "Represents shell command sequences separated by ;, &, &&, or ||.
   SEPARATORS is a list of :semi, :amp, :and, or :or keywords, one per command except the last."
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


;; -- Arg utilities (cons-based arg support) -----------------
(defun arg-value (arg)
  "Extract string value from an arg (string or (value . quote-style) cons)."
  (if (consp arg) (car arg) arg))

(defun arg-quote-style (arg)
  "Return the quote style of ARG: :SINGLE, :DOUBLE, or NIL (unquoted).
Bare-string args and redirect-target conses are unquoted."
  (and (consp arg) (cdr arg)))

(defun arg-quoted-p (arg)
  "Return T only when ARG was single-quoted and must not be expanded at all.
Double-quoted args still undergo variable/command expansion (but no globbing),
so they are deliberately excluded here."
  (let ((style (arg-quote-style arg)))
    ;; Treat legacy T (older single-quote encoding) as :SINGLE for safety.
    (or (eq style :single) (eq style t))))

(defun command-node-arg-values (node)
  "Return all args as plain strings (unwrapping cons cells)."
  (mapcar #'arg-value (command-node-args node)))
