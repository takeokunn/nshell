(in-package #:nshell.application)

(defun %format-function-definition (name body-lines)
  (with-output-to-string (out)
    (format out "function ~a~%" name)
    (dolist (line body-lines)
      (format out "  ~a~%" line))
    (format out "end~%")))

(defun %function-body-args (args)
  (if (and (rest args)
           (string= (car (last args)) "end"))
      (butlast (rest args))
      (rest args)))

(defun %store-function-body (table name args)
  (let ((body-line (%join-command-args (%function-body-args args))))
    (setf (gethash name table)
          (if (string= body-line "")
              nil
              (list body-line)))
    (values nil 0)))

(defun %format-functions (table &optional names)
  (%format-name-table
   table
   (lambda (out name body-lines)
     (write-string (%format-function-definition name body-lines) out))
   names))

(defun %builtin-function (context args)
  (let ((table (shell-context-function-table context)))
    (%table-builtin-case args
      (:empty
       (values (%format-functions table) 0))
      (:option ("-e" "--erase")
       (%with-required-argument (%builtin-function args "function" "-e" "a name" 2)
         (%table-erase-names table (rest args))))
      (:option ("-q" "--query")
       (values nil (%table-query-status table (rest args))))
      (:default
       (if (rest args)
           (%store-function-body table (first args) args)
           (values (%format-functions table args)
                   (%table-query-status table (list (first args)))))))))
