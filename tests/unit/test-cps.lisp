(in-package #:nshell/test)
(def-suite cps-tests :description "CPS trampoline tests" :in nshell-tests)
(in-suite cps-tests)
(test trampoline-sequential
  (let ((results '()))
    (nshell.presentation:trampoline
     (lambda () (push 1 results)
       (lambda () (push 2 results)
         (lambda () (push 3 results) (nshell.presentation:done)))))
    (is (equal '(3 2 1) results))))
(test trampoline-termination
  (is (null (nshell.presentation:done))))
