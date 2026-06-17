(in-package #:nshell/test)

(def-suite pty-foreground-integration-tests
  :description "Foreground PTY process integration tests"
  :in nshell-tests)

(in-suite pty-foreground-integration-tests)

(defun pty-test-read-available (fd &key (timeout-usec 500000) (limit 4096))
  (let ((buffer (make-array limit :element-type '(unsigned-byte 8))))
    (sb-alien:with-alien ((read-fds (sb-alien:struct sb-unix:fd-set)))
      (sb-unix:fd-zero (sb-alien:addr read-fds))
      (sb-unix:fd-set fd (sb-alien:addr read-fds))
      (multiple-value-bind (ready errno)
          (sb-unix:unix-fast-select (1+ fd) (sb-alien:addr read-fds) nil nil 0 timeout-usec)
        (declare (ignore errno))
        (when (and ready (plusp ready) (sb-unix:fd-isset fd (sb-alien:addr read-fds)))
          (let ((count (ignore-errors
                         (nshell.infrastructure.acl:pty-read fd buffer limit))))
            (when (and count (plusp count))
              (octets->string buffer count))))))))

(defun pty-test-read-until (fd needle &key (attempts 20))
  (let ((output ""))
    (dotimes (i attempts output)
      (let ((chunk (pty-test-read-available fd)))
        (when chunk
          (setf output (concatenate 'string output chunk))
          (when (search needle output :test #'char-equal)
            (return output)))))))

(defun pty-test-close-process (pty)
  (when pty
    (ignore-errors
      (nshell.infrastructure.acl:kill-process
       (- (nshell.infrastructure.acl:pty-process-pgid pty)) :sigterm))
    (ignore-errors (close (nshell.infrastructure.acl:pty-process-stream pty)))))

(test pty-spawn-creates-process-with-master-fd
  "PTY-SPAWN starts a subprocess and exposes its PTY master fd."
  #-(or darwin linux)
  (skip "PTY tests are only supported on Darwin and Linux")
  #+(or darwin linux)
  (let ((pty nil))
    (unwind-protect
         (progn
           (setf pty (nshell.infrastructure.acl:pty-spawn "/bin/sh" '("-c" "echo pty-ready")))
           (is (nshell.infrastructure.acl:pty-process-p pty))
           (is (plusp (nshell.infrastructure.acl:pty-process-pid pty)))
           (is (integerp (nshell.infrastructure.acl:pty-process-master-fd pty)))
           (is (search "pty-ready"
                       (pty-test-read-until
                        (nshell.infrastructure.acl:pty-process-master-fd pty)
                        "pty-ready"))))
      (pty-test-close-process pty))))

(test pty-basic-io-roundtrip-through-cat
  "PTY master can drive an interactive child with bidirectional I/O."
  #-(or darwin linux)
  (skip "PTY tests are only supported on Darwin and Linux")
  #+(or darwin linux)
  (let ((pty nil))
    (unwind-protect
         (progn
           (setf pty (nshell.infrastructure.acl:pty-spawn "/bin/cat" '()))
           (nshell.infrastructure.acl:pty-write
            (nshell.infrastructure.acl:pty-process-master-fd pty)
            (string->octets (line "hello-from-pty")))
           (is (search "hello-from-pty"
                       (pty-test-read-until
                        (nshell.infrastructure.acl:pty-process-master-fd pty)
                        "hello-from-pty"))))
      (pty-test-close-process pty))))

(test pty-spawn-propagates-window-size
  "PTY-SPAWN propagates rows/cols to the child terminal."
  #-(or darwin linux)
  (skip "PTY tests are only supported on Darwin and Linux")
  #+(or darwin linux)
  (let ((pty nil))
    (unwind-protect
         (progn
           (setf pty (nshell.infrastructure.acl:pty-spawn
                      "/bin/sh" '("-c" "sleep 0.1; stty size")
                      :rows 37 :cols 123))
           (is (search "37 123"
                       (pty-test-read-until
                        (nshell.infrastructure.acl:pty-process-master-fd pty)
                        "37 123"))))
      (pty-test-close-process pty))))

(test pty-foreground-suspend-resume
  "A stopped PTY foreground process can be continued and observed to exit."
  #-(or darwin linux)
  (skip "PTY tests are only supported on Darwin and Linux")
  #+(or darwin linux)
  (let ((pty nil))
    (unwind-protect
         (progn
           (setf pty (nshell.infrastructure.acl:pty-spawn
                      "/bin/sh" '("-c" "kill -STOP $$; echo resumed")))
           (loop repeat 20
                 for status = (multiple-value-list
                               (nshell.infrastructure.acl:wait-job
                                (nshell.infrastructure.acl:pty-process-pid pty)
                                :untraced t
                                :nohang t))
                 when (eq (second status) :stopped)
                   do (return)
                 do (sleep 0.05))
           (nshell.infrastructure.acl:kill-process
            (- (nshell.infrastructure.acl:pty-process-pgid pty)) :sigcont)
           (is (search "resumed"
                       (pty-test-read-until
                        (nshell.infrastructure.acl:pty-process-master-fd pty)
                        "resumed")))
           (loop repeat 20
                 for status = (multiple-value-list
                               (nshell.infrastructure.acl:wait-job
                                (nshell.infrastructure.acl:pty-process-pid pty)
                                :nohang t))
                 when (member (second status) '(:exited :signaled :no-child))
                   do (return)
                 do (sleep 0.05)))
      (pty-test-close-process pty))))
