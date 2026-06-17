(in-package #:nshell/test)

(defun repl-completion-texts (candidates)
  (mapcar #'nshell.domain.completion:candidate-text candidates))

(defun repl-ansi (suffix)
  (format nil "~C~a" #\Esc suffix))

(defmacro with-repl-test-state (&body body)
  `(let ((nshell.presentation::*running* t)
         (nshell.presentation::*last-exit-code* 0)
         (nshell.presentation::*history* (nshell.domain.history:make-command-history))
         (nshell.presentation::*config* nil)
         (nshell.presentation::*kb* (nshell.domain.completion:make-knowledge-base))
         (nshell.presentation::*input-state* nil)
         (nshell.presentation::*completion-rendered-lines* 0)
         (nshell.presentation::*prompt-rendered-lines* 0)
         (nshell.presentation::*prompt-rendered-cursor-row* 0)
         (nshell.presentation::*environment* (nshell.domain.environment:make-environment))
         (nshell.presentation::*aliases* (make-hash-table :test #'equal))
         (nshell.presentation::*abbreviations* (make-hash-table :test #'equal))
         (nshell.presentation::*functions* (make-hash-table :test #'equal))
         (nshell.presentation::*proc-registry* (make-hash-table :test #'eql)))
     ,@body))

(defmacro with-temporary-function ((symbol function) &body body)
  `(let ((original-function (symbol-function ,symbol)))
     (unwind-protect
          (progn
            (setf (symbol-function ,symbol) ,function)
            ,@body)
       (setf (symbol-function ,symbol) original-function))))

(defmacro with-stable-repl-prompt ((&key (width 4) (text "ns> ")) &body body)
  `(with-temporary-function
       ('nshell.presentation::render-prompt
        (lambda (config last-exit &key terminal-width)
          (declare (ignore config last-exit terminal-width))
          (format t "~a" ,text)
          ,width))
     ,@body))

(defmacro with-fixed-terminal-size ((rows columns) &body body)
  `(with-temporary-function
       ('nshell.infrastructure.acl:get-terminal-size
        (lambda () (values ,rows ,columns)))
     ,@body))

(defmacro with-repl-input-state (initargs &body body)
  `(let ((nshell.presentation::*input-state* (input-state ,@initargs)))
     ,@body))

(defmacro with-repl-render-state (input-initargs &body body)
  `(let ((nshell.presentation::*config*
           (nshell.domain.configuration:default-config))
         (nshell.domain.prompting:*git-status-resolver*
           (lambda (dir)
             (declare (ignore dir))
             (values nil nil))))
     (with-repl-input-state ,input-initargs
       ,@body)))

(defun call-repl-builtin (command args)
  (let ((builtin-p nil)
        (code nil)
        (output nil))
    (setf output
          (with-output-to-string (*standard-output*)
            (multiple-value-setq (builtin-p code)
              (nshell.presentation::execute-builtin
               (nshell.domain.parsing:make-command-node command args)))))
    (values output builtin-p code)))
