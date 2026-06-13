(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun copy-stream (in out)
  "Copy bytes from IN to OUT preserving exact content."
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf in)
          while (plusp n)
          do (write-sequence buf out :end n))))

(defun run-external (cmd args)
  "Run external command, print its output, return exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :output :stream :error :output :wait t :search t)))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out
              (copy-stream out *standard-output*)))
          (sb-ext:process-exit-code proc)))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      1)))

(defun spawn-pipeline (commands)
  "Execute commands as pipeline with byte-correct stream connections."
  (let* ((n (length commands))
         (procs nil)
         (prev-output nil))
    (loop for i from 0 below n
          for cmd-node in commands
          for cmd = (nshell.domain.parsing:command-node-command cmd-node)
          for args = (nshell.domain.parsing:command-node-args cmd-node)
          do (let ((proc (handler-case
                             (sb-ext:run-program cmd args
                               :input (if prev-output :stream t)
                               :output :stream
                               :error :output
                               :wait nil :search t)
                           (error (err)
                             (format *error-output* "nshell: ~a: ~a~%" cmd err)
                             nil))))
               (when proc
                 (when prev-output
                   (let ((in (sb-ext:process-input proc)))
                     (when in
                       (handler-case (copy-stream prev-output in)
                         (error ()))
                       (close in))))
                 (push proc procs)
                 (setf prev-output (sb-ext:process-output proc)))))
    (let ((exit 0))
      (dolist (proc (reverse procs))
        (when prev-output
          (handler-case (copy-stream prev-output *standard-output*)
            (error ()))
          (setf prev-output nil))
        (sb-ext:process-wait proc)
        (setf exit (or (sb-ext:process-exit-code proc) 0)))
      exit)))

(defun wait-job (process)
  (sb-ext:process-wait process)
  (sb-ext:process-exit-code process))

(defvar *redirected-stdout* nil)
(defvar *redirected-stdin* nil)

(defun redirect-output (filename mode)
  "Redirect stdout to FILENAME. MODE is :supersede or :append."
  (let ((stream (open filename :direction :output :if-exists mode
                      :if-does-not-exist :create)))
    (setf *redirected-stdout* *standard-output*)
    (setf *standard-output* stream)))

(defun redirect-input (filename)
  "Redirect stdin from FILENAME."
  (let ((stream (open filename :direction :input :if-does-not-exist :error)))
    (setf *redirected-stdin* *standard-input*)
    (setf *standard-input* stream)))

(defun restore-redirects ()
  "Restore stdout/stdin after redirect."
  (when *redirected-stdout*
    (close *standard-output*)
    (setf *standard-output* *redirected-stdout*)
    (setf *redirected-stdout* nil))
  (when *redirected-stdin*
    (close *standard-input*)
    (setf *standard-input* *redirected-stdin*)
    (setf *redirected-stdin* nil)))
