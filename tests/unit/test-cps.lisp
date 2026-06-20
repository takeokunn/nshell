(in-package #:nshell/test)
(def-suite cps-tests :description "CPS trampoline tests" :in nshell-tests)
(in-suite cps-tests)
(test trampoline-sequential
  (let ((results '()))
    (nshell.presentation:trampoline
     (lambda () (push 1 results)
       (lambda () (push 2 results)
         (lambda () (push 3 results) nil))))
    (is (equal '(3 2 1) results))))

(test trampoline-stops-after-done
  (let ((results '()))
    (nshell.presentation:trampoline
     (lambda ()
       (push :start results)
       nil))
    (is (equal '(:start) results))))

(test pbt-trampoline-preserves-continuation-order
  (check-property (:trials 50)
      ((depth (gen-in-range 1 8) nil))
    (let ((results '()))
      (labels ((walk-continuations (n)
                (if (zerop n)
                    nil
                    (lambda ()
                       (push n results)
                       (walk-continuations (1- n))))))
        (nshell.presentation:trampoline
         (lambda () (walk-continuations depth)))
        (equal (loop for i from 1 to depth collect i)
               results)))))

(test trampoline-termination
  (is (null nil)))
