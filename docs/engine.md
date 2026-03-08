# Iterative Engine Regression Tests

This module adds focused regression coverage for the explicit-stack evaluator.

```emacs-lisp
(ert-deftest eprolog-engine-basic-iterative-resolution ()
  "Basic facts, rules, bindings, and enumeration still work on the iterative engine."
  (eprolog-test--restore-builtins)

  (eprolog-define-predicate! (edge a b))
  (eprolog-define-predicate (edge b c))
  (eprolog-define-predicate! (path _x _y) (edge _x _y))
  (eprolog-define-predicate (path _x _y) (edge _x _z) (path _z _y))

  (should (eprolog-test--has-solution-p '((edge a b))))
  (let ((solutions (eprolog-test--collect-solutions '((path a _x)))))
    (should (= (length solutions) 2))
    (should (equal (mapcar (lambda (sol) (cdr (assoc '_x sol))) solutions)
                   '(b c)))))

(ert-deftest eprolog-engine-call-cut-scope ()
  "Cut inside call/1 should prune inner choices without discarding outer clauses."
  (eprolog-test--restore-builtins)

  (eprolog-define-predicate! (choice a))
  (eprolog-define-predicate (choice b))

  (eprolog-define-predicate! (via-call _x)
    (call (and (choice _x) !)))
  (eprolog-define-predicate (via-call fallback))

  (let ((solutions (eprolog-test--collect-solutions '((via-call _x)))))
    (should (= (length solutions) 2))
    (should (equal (mapcar (lambda (sol) (cdr (assoc '_x sol))) solutions)
                   '(a fallback)))))

(ert-deftest eprolog-engine-dynamic-parameters-backtrack ()
  "Dynamic parameter snapshots must restore from explicit choice points."
  (eprolog-test--restore-builtins)

  (let ((solutions (eprolog-test--collect-solutions
                    '((store mode base)
                      (or (and (store mode branch) (fetch mode _value))
                          (fetch mode _value))))))
    (should (= (length solutions) 2))
    (should (equal (mapcar (lambda (sol) (cdr (assoc '_value sol))) solutions)
                   '(branch base)))))

(ert-deftest eprolog-engine-stack-safety-member-append-repeat ()
  "Deep search paths should run without growing the Lisp call stack."
  (eprolog-test--restore-builtins)

  (let ((deep-list (number-sequence 1 400)))
    (should (eprolog-test--has-solution-p `((member 400 ,deep-list)))))

  (let* ((target (number-sequence 1 40))
         (solutions (eprolog-test--collect-solutions `((append _left _right ,target)))))
    (should (= (length solutions) 41)))

  (let ((counter 0))
    (eprolog-define-predicate! (repeat-until _limit)
      (repeat)
      (lisp! (setq counter (1+ counter)))
      (lispp (>= counter _limit))
      !)
    (should (eprolog-test--has-solution-p '((repeat-until 300))))
    (should (= counter 300))))

(ert-deftest eprolog-engine-stack-safety-dcg ()
  "Deep DCG parsing should use the iterative evaluator."
  (eprolog-test--restore-builtins)

  (eprolog-define-grammar! tokens [])
  (eprolog-define-grammar (tokens) [item] tokens)

  (let ((input (make-list 80 'item)))
    (should (eprolog-test--has-solution-p `((phrase tokens ,input)))))

  (let ((input (append (make-list 60 'item) '(stop))))
    (should (eprolog-test--has-solution-p `((phrase tokens ,input (stop)))))))

(ert-deftest eprolog-engine-step-limit-signals-error ()
  "Step limit exhaustion should signal an explicit engine error."
  (eprolog-test--restore-builtins)

  (let ((eprolog-max-steps 50))
    (eprolog-define-predicate! (infinite _x) (infinite _x))
    (should-error
     (eprolog-test--has-solution-p '((infinite a)))
     :type 'eprolog-step-limit-exceeded)))
```
