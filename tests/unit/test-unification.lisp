(in-package #:nshell/test)

(def-suite unification-tests
  :description "Unification engine tests"
  :in nshell-tests)

(in-suite unification-tests)

(test unify-atoms
  (let ((b (nshell.domain.parsing:unify 'foo 'foo)))
    (is (not (null b)))))

(test unify-different-atoms
  (let ((b (nshell.domain.parsing:unify 'foo 'bar)))
    (is (null b))))

(test unify-variable-with-value
  (let* ((x (nshell.domain.parsing:make-var "X"))
         (b (nshell.domain.parsing:unify x 'hello)))
    (is (not (null b))))
    (is (eq 'hello (nshell.domain.parsing:walk x b)))))

(test unify-lists
  (let* ((x (nshell.domain.parsing:make-var "X"))
         (b (nshell.domain.parsing:unify (list x 'b) '(a b))))
    (is (not (null b))))
    (is (eq 'a (nshell.domain.parsing:walk x b)))))

(test occurs-check
  (let* ((x (nshell.domain.parsing:make-var "X"))
         (b (nshell.domain.parsing:unify x (list x))))
    (is (null b))))

(test backtrack-simple
  (let* ((x (nshell.domain.parsing:make-var "X"))
         (goal (lambda (b) (nshell.domain.parsing:unify x 42 b)))
         (result (nshell.domain.parsing:backtrack (list goal))))
    (is (not (null result)))
    (is (= 42 (nshell.domain.parsing:walk x result)))))

(test walk-resolves-chain
  (let* ((x (nshell.domain.parsing:make-var "X"))
         (y (nshell.domain.parsing:make-var "Y"))
         (b1 (nshell.domain.parsing:unify x y))
         (b2 (nshell.domain.parsing:unify y 10 b1)))
    (is (= 10 (nshell.domain.parsing:walk x b2)))))
