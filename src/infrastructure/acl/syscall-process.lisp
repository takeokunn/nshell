(in-package #:nshell.infrastructure.acl)

(defun spawn-async (cmd args &key redirects)
  "Spawn CMD with ARGS asynchronously. Returns the SBCL process object, or NIL on error."
  (let ((redirect-streams nil))
    (unwind-protect
         (handler-case
             (let* ((input-target (%redirect-target redirects :<))
                    (input (if input-target
                               (let ((stream (open input-target
                                                   :direction :input
                                                   :if-does-not-exist :error)))
                                 (push stream redirect-streams)
                                 stream)
                               *standard-input*))
                    (output
                      (multiple-value-bind (output-target output-mode)
                          (%redirect-output-spec redirects)
                        (if output-target
                            (let ((stream (open output-target
                                                :direction :output
                                                :if-exists output-mode
                                                :if-does-not-exist :create)))
                              (push stream redirect-streams)
                              stream)
                            t)))
                    (proc (sb-ext:run-program cmd args
                            :input input
                            :output output
                            :error (if *redirected-stderr* *error-output* :output)
                            :wait nil
                            :search t
                            :environment (%get-environment))))
               (when proc
                 (let ((pid (sb-ext:process-pid proc)))
                   (when (plusp pid)
                     (handler-case (set-process-group pid pid)
                       (error ()))))
                 proc))
           (error (err)
             (format *error-output* "nshell: ~a: ~a~%" cmd err)
             nil))
      (dolist (stream redirect-streams)
        (ignore-errors (close stream))))))

(defun run-external (cmd args)
  "Execute CMD with ARGS synchronously, printing output. Returns exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :input *standard-input*
                    :output :stream
                    :error (if *redirected-stderr* *error-output* :output)
                    :wait t
                    :search t
                    :environment (%get-environment))))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out
              (loop for line = (read-line out nil nil)
                    while line
                    do (write-line line))))
          (or (sb-ext:process-exit-code proc) 0)))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      1)))

(defun run-external-capture (cmd args)
  "Execute CMD with ARGS synchronously. Returns captured output and exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :input *standard-input*
                    :output :stream
                    :error (if *redirected-stderr* *error-output* :output)
                    :wait nil
                    :search t
                    :environment (%get-environment))))
        (if proc
            (let ((out (sb-ext:process-output proc)))
              (let ((output (if out
                                (with-output-to-string (buffer)
                                  (loop for char = (read-char out nil nil)
                                        while char
                                        do (write-char char buffer)))
                                "")))
                (sb-ext:process-wait proc)
                (values output (or (sb-ext:process-exit-code proc) 0))))
            (values "" 1)))
    (error (err)
      (values (format nil "nshell: ~a: ~a~%" cmd err) 1))))
