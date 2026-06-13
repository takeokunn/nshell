(in-package #:nshell/test)

(def-suite pty-tests
  :description "PTY integration tests"
  :in nshell-tests)

(in-suite pty-tests)

(defun octets->string (octets count)
  (coerce (loop for i below count
                collect (code-char (aref octets i)))
          'string))

(defun string->octets (string)
  (let ((octets (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for i below (length string)
          do (setf (aref octets i) (char-code (char string i))))
    octets))

(defun line (text)
  (concatenate 'string text (string #\Newline)))

(test pty-open-write-read-close
  "PTY can be opened, used in both directions, and closed."
  #-(or darwin linux)
  (skip "PTY tests are only supported on Darwin and Linux")
  #+(or darwin linux)
  (multiple-value-bind (master slave slave-name) (nshell.infrastructure.acl:open-pty)
    (unwind-protect
         (progn
           (is (integerp master))
           (is (integerp slave))
           (is (stringp slave-name))
           (let ((from-master (make-array 64 :element-type '(unsigned-byte 8))))
             (nshell.infrastructure.acl:pty-write master (string->octets (line "master-to-slave")))
             (let ((count (nshell.infrastructure.acl:pty-read slave from-master 64)))
               (is (plusp count))
               (is (search "master-to-slave" (octets->string from-master count)))))
           (let ((from-slave (make-array 64 :element-type '(unsigned-byte 8))))
             (nshell.infrastructure.acl:pty-write slave (string->octets (line "slave-to-master")))
             (let ((count (nshell.infrastructure.acl:pty-read master from-slave 64)))
               (is (plusp count))
               (is (search "slave-to-master" (octets->string from-slave count))))))
      (nshell.infrastructure.acl:pty-close master slave))))

(test with-pty-binds-streams
  "WITH-PTY binds usable unbuffered streams."
  #-(or darwin linux)
  (skip "PTY tests are only supported on Darwin and Linux")
  #+(or darwin linux)
  (nshell.infrastructure.acl:with-pty (master slave slave-name)
    (is (streamp master))
    (is (streamp slave))
    (is (stringp slave-name))))
