(in-package #:nshell/test)

(def-suite signal-handling-tests
  :description "Signal handling integration tests"
  :in nshell-tests)

(in-suite signal-handling-tests)

(test signal-constants-and-mapping-exist
  "Signal constants can be mapped between OS and domain values."
  (is (nshell.domain.signals:signal-p
       (nshell.infrastructure.acl:os-signal->domain :sigint)))
  (is (nshell.domain.signals:signal-p
       (nshell.infrastructure.acl:os-signal->domain :sigchld)))
  (is (eq :sigint
          (nshell.infrastructure.acl:domain-signal->os nshell.domain.signals:+sigint+))))

(test install-signal-handlers-does-not-crash
  "Installing signal handlers should complete without killing the shell."
  (is (eq t (nshell.infrastructure.acl:install-signal-handlers))))

(test reap-children-empty-when-no-children
  "Reaping with no changed children returns an empty list."
  (is (listp (nshell.infrastructure.acl:reap-children))))
