(in-package #:nshell/test)

(def-suite environment-tests
  :description "Environment domain tests"
  :in nshell-tests)

(in-suite environment-tests)

(test env-set-and-get-roundtrip
  "Variables set in an environment can be retrieved."
  (let* ((env (nshell.domain.environment:make-environment))
         (updated (nshell.domain.environment:env-set env "FOO" "bar" nil)))
    (is (string= "bar" (nshell.domain.environment:env-get updated "FOO")))
    (is (null (nshell.domain.environment:env-get env "FOO")))))

(test env-unset-removes-variable
  "Unsetting a variable removes it from the environment."
  (let* ((env (nshell.domain.environment:make-environment))
         (with-var (nshell.domain.environment:env-set env "FOO" "bar" nil))
         (without-var (nshell.domain.environment:env-unset with-var "FOO")))
    (is (null (nshell.domain.environment:env-get without-var "FOO")))))

(test env-export-marks-existing-variable
  "Exporting a variable makes it appear in the exported environment list."
  (let* ((env (nshell.domain.environment:make-environment))
         (with-var (nshell.domain.environment:env-set env "FOO" "bar" nil))
         (exported (nshell.domain.environment:env-export with-var "FOO")))
    (is (equal '("FOO" . "bar")
               (assoc "FOO" (nshell.domain.environment:env-list exported) :test #'string=)))))

(test env-list-only-returns-exported-vars
  "Only exported variables are included in ENV-LIST."
  (let* ((env (nshell.domain.environment:make-environment))
         (env (nshell.domain.environment:env-set env "LOCAL" "no" nil))
         (env (nshell.domain.environment:env-set env "EXPORTED" "yes" t))
         (pairs (nshell.domain.environment:env-list env)))
    (is (null (assoc "LOCAL" pairs :test #'string=)))
    (is (equal '("EXPORTED" . "yes")
               (assoc "EXPORTED" pairs :test #'string=)))))

(test default-environment-has-core-variables
  "The default environment contains core shell variables."
  (let ((env (nshell.domain.environment:make-default-environment)))
    (dolist (name '("HOME" "PATH" "USER"))
      (is (stringp (nshell.domain.environment:env-get env name))))))
