(in-package #:nshell.infrastructure.acl)

(defvar *redirected-stdout* nil)
(defvar *redirected-stdin* nil)

(defun %redirect-target (redirects op)
  (cdr (find op redirects :key #'car :from-end t)))

(defun %redirect-output-spec (redirects)
  (let ((redirect (find-if (lambda (redirect)
                             (member (car redirect) '(:> :>>)))
                           redirects
                           :from-end t)))
    (when redirect
      (values (cdr redirect)
              (if (eq (car redirect) :>>) :append :supersede)))))

(defun redirect-output (filename mode)
  (let ((stream (open filename
                      :direction :output
                      :if-exists mode
                      :if-does-not-exist :create)))
    (setf *redirected-stdout* *standard-output*
          *standard-output* stream)))

(defun redirect-input (filename)
  (let ((stream (open filename :direction :input :if-does-not-exist :error)))
    (setf *redirected-stdin* *standard-input*
          *standard-input* stream)))

(defun restore-redirects ()
  (when *redirected-stdout*
    (close *standard-output*)
    (setf *standard-output* *redirected-stdout*
          *redirected-stdout* nil))
  (when *redirected-stdin*
    (close *standard-input*)
    (setf *standard-input* *redirected-stdin*
          *redirected-stdin* nil)))
