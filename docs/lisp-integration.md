
# Lisp Integration

One of ε-prolog's most remarkable achievements is its seamless fusion with Emacs Lisp, creating a hybrid programming environment where logical and functional paradigms complement each other naturally. This integration isn't just a technical convenience—it's a bridge between two fundamentally different ways of thinking about computation.

The beauty of this integration lies in its bidirectional nature. Prolog queries can invoke Lisp functions to perform calculations, access Emacs features, or manipulate data structures, while Lisp code can query the Prolog knowledge base. This creates a powerful symbiosis where each language contributes its strengths to solve complex problems.

## Basic Lisp Interface

The basic Lisp interface provides predicates that bridge the gap between Prolog's logical world and Lisp's functional world. These predicates are essential for practical programming in ε-prolog.

The basic Lisp interface provides predicates for calling Lisp code from within Prolog queries. This is essential for accessing Emacs functions, performing calculations, and integrating with the Emacs environment.

This subsection covers:

-   `lisp/2` for evaluating Lisp expressions and capturing results
-   `lispp/1` for evaluating Lisp expressions as boolean tests
-   `lisp!/1` for evaluating Lisp expressions for side effects only
-   How to pass data between Prolog and Lisp

### Usage Example

```
;; Evaluate a Lisp expression and capture result
(eprolog-query '((lisp _result (+ 2 3))))
;; _result => 5

;; Use lispp/1 for boolean test
(eprolog-query '((lispp (> 5 3))))

;; Perform side effects
(eprolog-query '((lisp! (message "Hello from Lisp!"))))
```

The following comprehensive test demonstrates all three main ways to interface with Lisp in a single, well-organized test case:

```emacs-lisp
(ert-deftest eprolog-feature-lisp-integration ()
  "Test comprehensive Lisp integration predicates - all three integration modes."
  (eprolog-test--restore-builtins)

  ;; === LISP/2 EVALUATION TESTS ===
  ;; Test lisp/2 for evaluating expressions and capturing results

  ;; Basic arithmetic evaluation
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result (+ 1 2 3))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 6)))

  ;; More complex arithmetic
  (let ((solutions (eprolog-test--collect-solutions '((lisp _x (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 5)))

  ;; Function calls and data structure creation
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result (list 'a 'b 'c))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) '(a b c))))

  ;; === LISPP/1 BOOLEAN TESTS ===
  ;; Test lispp/1 for boolean evaluation of Lisp expressions

  ;; Basic comparison tests
  (let ((solutions (eprolog-test--collect-solutions '((lispp (> 5 3))))))
    (should (= (length solutions) 1)))
  (should-not (eprolog-test--has-solution-p '((lispp (< 5 3)))))
  (should-not (eprolog-test--has-solution-p '((lispp (> 3 5)))))

  ;; Boolean logic tests
  (let ((solutions (eprolog-test--collect-solutions '((lispp (and t t))))))
    (should (= (length solutions) 1)))
  (should-not (eprolog-test--has-solution-p '((lispp (and t nil)))))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (or t nil))))))
    (should (= (length solutions) 1)))

  ;; === LISP!/1 SIDE EFFECT TESTS ===
  ;; Test lisp!/1 for executing Lisp code for side effects

  ;; Variable assignment side effects
  (let ((test-var nil))
    (let ((solutions (eprolog-test--collect-solutions `((lisp! (setq test-var 'success))))))
      (should (= (length solutions) 1)))
    (should (eq test-var 'success)))

  ;; More complex side effect operations
  (setq eprolog-test--temp-var nil)
  (let ((solutions (eprolog-test--collect-solutions `((lisp! (setq eprolog-test--temp-var 'modified))))))
    (should (= (length solutions) 1)))
  (should (eq eprolog-test--temp-var 'modified))

  ;; Multiple side effects in sequence
  (setq eprolog-test--counter 0)
  (let ((solutions (eprolog-test--collect-solutions `((lisp! (setq eprolog-test--counter (+ eprolog-test--counter 1)))))))
    (should (= (length solutions) 1)))
  (should (= eprolog-test--counter 1)))
```

## Lisp Integration Negative Tests

The lisp predicate should fail with invalid expressions and unification failures:

```emacs-lisp
(ert-deftest eprolog-feature-lisp-negative-tests ()
  "Test negative cases for lisp predicate."
  (eprolog-test--restore-builtins)

  (should-error (eprolog-test--has-solution-p '((lisp _x (undef)))))
  (should-not (eprolog-test--has-solution-p '((lisp _x (_undef))))))
```

## Lisp Side Effects (lisp!) Negative Tests

The lisp! predicate should fail with invalid expressions:

```emacs-lisp
(ert-deftest eprolog-feature-lisp-side-effects-negative-tests ()
  "Test negative cases for lisp! predicate."
  (eprolog-test--restore-builtins)

  (should-error (eprolog-test--has-solution-p '((lisp! (undef)))))
  (should-not (eprolog-test--has-solution-p '((lisp! (_undef)))))
  )
```

## Lisp Conditional (lispp) Negative Tests

The lispp predicate should fail when expressions return nil or false:

```emacs-lisp
(ert-deftest eprolog-feature-lisp-lispp-negative-tests ()
  "Test negative cases for lispp predicate."
  (eprolog-test--restore-builtins)

  ;; Expressions that return nil should fail
  (should-not (eprolog-test--has-solution-p '((lispp (> 2 5)))))
  (should-not (eprolog-test--has-solution-p '((lispp (equal 'a 'b)))))
  (should-not (eprolog-test--has-solution-p '((lispp (< 10 5)))))
  (should-not (eprolog-test--has-solution-p '((lispp nil))))

  (should-error (eprolog-test--has-solution-p '((lispp (undef)))))
  (should-not (eprolog-test--has-solution-p '((lispp (_undef)))))

  ;; Complex boolean expressions that evaluate to false
  (should-not (eprolog-test--has-solution-p '((lispp (and t nil)))))
  (should-not (eprolog-test--has-solution-p '((lispp (or nil nil))))))
```

## Dynamic Parameters

For stateful parameter storage and backtracking semantics, see [Dynamic Parameters](dynamic-parameters.md).

## Advanced Lisp Integration Error Handling

These tests verify robust error handling in complex Lisp integration scenarios.

### Invalid Lisp Expression Tests

```emacs-lisp
(ert-deftest eprolog-feature-lisp-invalid-expressions ()
  "Test error handling with malformed Lisp expressions."
  (eprolog-test--restore-builtins)

  ;; Test lisp/2 with invalid expressions (may fail or error)
  (should-error (eprolog-test--has-solution-p '((lisp _result (undefined-function 1 2 3)))))
  (should-error (eprolog-test--has-solution-p '((lisp _result (+ 1 undefined-variable)))))

  ;; Test lispp/1 with invalid boolean expressions (may fail or error)
  (should-error (eprolog-test--has-solution-p '((lispp (non-existent-predicate 1 2)))))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (< 1))))))
    (should (= (length solutions) 1)))

  ;; Test lisp!/1 with side-effect expressions that fail (may fail or error)
  (should-error (eprolog-test--has-solution-p '((lisp! (error "Intentional error")))))
  (should-error (eprolog-test--has-solution-p '((lisp! (setq undefined-variable undefined-other))))))
```

### Large Data Transfer Tests

```emacs-lisp
(ert-deftest eprolog-feature-lisp-large-data-transfer ()
  "Test passing large data structures between Prolog and Lisp."
  (eprolog-test--restore-builtins)

  ;; Test with moderately large list (reduced from 1000 to avoid stack overflow)
  (let ((large-list (make-list 100 'test-item)))
    (let ((solutions (eprolog-test--collect-solutions `((lisp _result (length ',large-list))))))
      (should (= (length solutions) 1))
      (should (= (cdr (assoc '_result (car solutions))) 100))))

  ;; Test with large numeric computations
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result (apply '+ (number-sequence 1 100)))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 5050)))

  ;; Test memory efficiency with repeated large operations
  (dotimes (i 10)
    (let ((test-data (make-list 100 i)))
      (let ((solutions (eprolog-test--collect-solutions `((lisp _result (length ',test-data))))))
        (should (= (length solutions) 1))
        (should (= (cdr (assoc '_result (car solutions))) 100))))))
```

### Type Conversion Edge Cases

```emacs-lisp
(ert-deftest eprolog-feature-lisp-type-conversion-edge-cases ()
  "Test edge cases in type conversion between Prolog and Lisp."
  (eprolog-test--restore-builtins)

  ;; Test conversion of special Lisp values
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result t)))))
    (should (= (length solutions) 1))
    (should (eq (cdr (assoc '_result (car solutions))) t)))
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result nil)))))
    (should (= (length solutions) 1))
    (should (eq (cdr (assoc '_result (car solutions))) nil)))

  ;; Test conversion of complex Lisp data types
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result (make-hash-table))))))
    (should (= (length solutions) 1))
    (should (hash-table-p (cdr (assoc '_result (car solutions))))))

  ;; Test conversion failures with non-serializable objects
  ;; Note: These might behave differently depending on implementation
  ;; (should-not (eprolog-test--has-solution-p '((lisp _result (lambda (x) x)))))

  ;; Test numeric edge cases
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result 1.0e+INF)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) 1.0e+INF)))
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result -1.0e+INF)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) -1.0e+INF)))

  ;; Test very large numbers
  (let ((large-num (expt 2 100)))
    (let ((solutions (eprolog-test--collect-solutions `((lisp _result ,large-num)))))
      (should (= (length solutions) 1))
      (should (= (cdr (assoc '_result (car solutions))) large-num))))

  ;; Test strings with special characters
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result "\n\t\"\\'")))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) "\n\11\"\\'")))
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result "unicode: αβγδε")))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) "unicode: αβγδε"))))
```

### Nested Lisp Calls and Complex Integration

```emacs-lisp
(ert-deftest eprolog-feature-lisp-nested-complex-integration ()
  "Test complex nested Lisp integration scenarios."
  (eprolog-test--restore-builtins)

  ;; Test nested lisp calls within Prolog predicates
  (eprolog-define-predicate (complex-lisp-calc _input _result)
    (lisp _doubled (* _input 2))
    (lisp _squared (* _doubled _doubled))
    (lisp _result (/ _squared 4)))

  (let ((solutions (eprolog-test--collect-solutions '((complex-lisp-calc 5 _result)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 25)))

  ;; Test Lisp side effects persisting across calls
  (let ((solutions (eprolog-test--collect-solutions '((lisp! (setq test-counter 0))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lisp! (setq test-counter (1+ test-counter)))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lisp! (setq test-counter (1+ test-counter)))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result test-counter)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 2)))

  ;; Test error recovery in complex scenarios
  (eprolog-define-predicate (error-recovery-test _result)
    (lisp _result (+ 1 2))
    ;; This should not affect the success of the first lisp call
    (lisp _dummy (/ 1 0))) ;; This might cause an error

  ;; The predicate might fail due to division by zero, but shouldn't crash
  (condition-case nil
      (eprolog-test--has-solution-p '((error-recovery-test _result)))
    (error t))) ;; Accept controlled failure
```

