(in-package #:nshell/test)

(def-suite signal-tests
  :description "Signal value object tests"
  :in nshell-tests)

(in-suite signal-tests)

(test signal-creation
  "Signals can be created with name and number"
  (let ((sig (nshell.domain.signals:make-signal :sigint 2)))
    (is (nshell.domain.signals:signal-p sig))
    (is (eq :sigint (nshell.domain.signals:signal-name sig)))
    (is (= 2 (nshell.domain.signals:signal-number sig)))))

(test signal-equality
  "Signals are equal when name and number match"
  (let ((a (nshell.domain.signals:make-signal :sigterm 15))
        (b (nshell.domain.signals:make-signal :sigterm 15))
        (c (nshell.domain.signals:make-signal :sigint 2)))
    (is (nshell.domain.signals:signal= a b))
    (is (not (nshell.domain.signals:signal= a c)))))

(test known-signal-constants
  "All predefined signal constants have expected values"
  (is (= 2  (nshell.domain.signals:signal-number nshell.domain.signals:+sigint+)))
  (is (= 15 (nshell.domain.signals:signal-number nshell.domain.signals:+sigterm+)))
  (is (= 20 (nshell.domain.signals:signal-number nshell.domain.signals:+sigtstp+)))
  (is (= 18 (nshell.domain.signals:signal-number nshell.domain.signals:+sigcont+)))
  (is (= 17 (nshell.domain.signals:signal-number nshell.domain.signals:+sigchld+))))
