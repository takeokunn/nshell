(in-package #:nshell/test)
(def-suite pipeline-plan-tests :description "Pipeline plan tests" :in nshell-tests)
(in-suite pipeline-plan-tests)
(test empty-pipeline-detected
  (let ((pipe (nshell.domain.execution:make-pipeline)))
    (is (nshell.domain.execution:pipeline-empty-p pipe))))
(test single-command-pipeline
  (let* ((cmd (nshell.domain.execution:make-command "ls"))
         (pipe (nshell.domain.execution:make-pipeline cmd)))
    (is (nshell.domain.execution:pipeline-single-command-p pipe))))
