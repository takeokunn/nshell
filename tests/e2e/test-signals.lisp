(in-package #:nshell/test)
(def-suite e2e-signal-tests :description "E2E signal tests" :in nshell-tests)
(in-suite e2e-signal-tests)
(test e2e-signal-constants-exist
  (is (nshell.domain.signals:signal-p nshell.domain.signals:+sigint+))
  (is (nshell.domain.signals:signal-p nshell.domain.signals:+sigterm+)))
