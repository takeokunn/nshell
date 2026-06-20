;;; nshell test runner
;;; Aggregates and runs all test suites

(in-package #:nshell/test)

(defun in-hermetic-sandbox-p ()
  "True when running inside a hermetic Nix build sandbox, where real OS process
and PTY facilities (/bin/sh, /bin/cat, a working /dev/pts) are unavailable. Such
integration tests are skipped here and exercised by the non-sandboxed CI job."
  (and (uiop:getenv "NIX_BUILD_TOP") t))

(defmacro skip-in-sandbox (reason &body body)
  "Run BODY only when not in a hermetic sandbox; otherwise skip with REASON."
  `(if (in-hermetic-sandbox-p)
       (skip "~a (skipped in hermetic sandbox)" ,reason)
       (progn ,@body)))

(def-suite nshell-tests
  :description "nshell test suite - all tests")

(in-suite nshell-tests)

(test smoke-test
  "Basic sanity check that the test framework and project are loaded correctly."
  (is (= 1 1))
  (is (string= "nshell" "nshell")))

(test main-cli-action
  "CLI argument dispatch should recognize help, version, and invalid inputs."
  (is (eq :help (nshell::%cli-action '("--help"))))
  (is (eq :help (nshell::%cli-action '("-h"))))
  (is (eq :version (nshell::%cli-action '("--version"))))
  (is (eq :version (nshell::%cli-action '("-V"))))
  (is (eq :command (nshell::%cli-action '("-c" "echo hello"))))
  (is (eq :command (nshell::%cli-action '("--command" "echo hello"))))
  (is (eq :run (nshell::%cli-action nil)))
  (is (eq :invalid (nshell::%cli-action '("script"))))
  (is (eq :invalid (nshell::%cli-action '("-c"))))
  (is (eq :invalid (nshell::%cli-action '("--unknown")))))

(test main-cli-output
  "Top-level text should include a usage line and version banner."
  (let ((usage (with-output-to-string (stream)
                 (nshell::%print-usage stream)))
        (version (with-output-to-string (stream)
                   (nshell::%print-version stream))))
    (is (search "Usage: nshell [--help] [--version] [-c COMMAND]" usage))
    (is (search "stdin is a terminal" usage))
    (is (search "With -c/--command" usage))
    (is (search "nshell v" version)))
  )

(defun run-tests ()
  "Run all nshell tests."
  (run! 'nshell-tests))
