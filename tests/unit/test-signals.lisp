(in-package #:nshell/test)

(def-suite signal-tests
  :description "Signal value object tests"
  :in nshell-tests)

(in-suite signal-tests)

(test signal-creation
  (let ((sig (nshell.domain.signals:make-signal :sigint 2)))
    (is (nshell.domain.signals:signal-p sig))))

(test signal-equality
  (let ((a (nshell.domain.signals:make-signal :sigterm 15))
        (b (nshell.domain.signals:make-signal :sigterm 15))
        (c (nshell.domain.signals:make-signal :sigint 2)))
    (is (nshell.domain.signals:signal= a b))
    (is (not (nshell.domain.signals:signal= a c)))))

(test known-signal-constants
  (is (nshell.domain.signals:signal-p nshell.domain.signals:+sigint+))
  (is (nshell.domain.signals:signal-p nshell.domain.signals:+sigterm+))
  (is (nshell.domain.signals:signal-p nshell.domain.signals:+sigcont+))
  (is (nshell.domain.signals:signal-p nshell.domain.signals:+sigchld+)))
