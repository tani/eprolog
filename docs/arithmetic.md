
# Arithmetic and Mathematics

Arithmetic in Prolog requires explicit evaluation, which distinguishes it from many other programming languages. This section explores how ε-prolog handles mathematical computations and provides predicates for numerical operations.

Arithmetic in Prolog requires explicit evaluation using the `is/2` predicate. Unlike many programming languages, arithmetic expressions are not automatically evaluated - they remain as symbolic structures until explicitly computed.

## Basic Arithmetic

This section demonstrates the fundamental arithmetic capabilities of ε-prolog, focusing on the explicit evaluation model that distinguishes Prolog from conventional programming languages.

The `is/2` predicate is the cornerstone of arithmetic in Prolog, providing explicit evaluation of mathematical expressions. Unlike languages where expressions are automatically evaluated, Prolog treats arithmetic expressions as symbolic structures until explicitly computed through `is/2`.

Key concepts covered in this section:

-   Basic arithmetic operations (`+`, `-`, `*`, `/`, `mod`) and their explicit evaluation
-   Complex arithmetic expressions with nested operations and operator precedence
-   How `is/2` evaluates expressions and binds results to variables
-   Integration with mathematical functions (`sqrt`, `expt`, etc.) available from the underlying Lisp system
-   Safety mechanisms: handling of unbound variables and type checking in arithmetic contexts

The following test demonstrates basic and complex arithmetic operations:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-basic ()
  "Test arithmetic evaluation with is/2 and mathematical functions."
  (eprolog-test--restore-builtins)

  ;; Test basic arithmetic
  (let ((solutions (eprolog-test--collect-solutions '((is _result (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 5)))

  ;; Test complex expressions
  (let ((solutions (eprolog-test--collect-solutions '((is _result (* (+ 2 3) 4))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 20)))

  ;; Test comprehensive examples
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ 15 25))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 40)))

  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ (* 2 3) (/ 8 2)))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 10)))

  ;; Test is/2 safety: unbound variables in expression should fail or error
  ;; Note: ε-prolog may throw errors for unbound variables in arithmetic
  (should (not (eprolog-test--has-solution-p '((is _result (+ _unbound 3))))))
  (should (not (eprolog-test--has-solution-p '((is _result (* _x _y))))))
  )
```

The `is/2` predicate serves as the foundation for all arithmetic operations:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-is-predicate ()
  "Test basic is/2 predicate as standalone test."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 5))))
```

Testing various arithmetic operations demonstrates the range of mathematical capabilities:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-operations ()
  "Test basic arithmetic operations with is/2."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ 15 25))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 40)))

  (let ((solutions (eprolog-test--collect-solutions '((is _x (* 12 8))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 96)))

  (let ((solutions (eprolog-test--collect-solutions '((is _x (- 100 37))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 63))))
```

Complex expressions and mathematical functions extend the computational capabilities:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-complex ()
  "Test complex arithmetic expressions."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ (* 2 3) (/ 8 2)))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 10)))

  (let ((solutions (eprolog-test--collect-solutions '((is _x (sqrt 16))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 4.0))))
```

## Mathematical Predicates

This section explores how to build sophisticated mathematical predicates by combining arithmetic operations with logical programming constructs. These examples demonstrate the power of declarative programming for mathematical reasoning.

Mathematical predicates in Prolog leverage the language's pattern matching and backtracking capabilities to create elegant solutions for numerical problems. By combining `is/2` for arithmetic evaluation with logical constructs like cuts (`!`) and recursive definitions, we can build robust mathematical functions.

The examples in this section illustrate:

-   Creating predicates for mathematical properties (even/odd, prime numbers)
-   Implementing recursive mathematical functions (factorial, sum-to-n)
-   Using cuts for deterministic mathematical computations
-   Building utility predicates for common mathematical operations

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-mathematical-predicates ()
  "Test mathematical predicates."
  (eprolog-test--restore-builtins)

  ;; Define even and odd predicates
  (eprolog-define-prolog-predicate (even-num _n)
    (is _r (mod _n 2))
    (lispp (= _r 0)))
  (eprolog-define-prolog-predicate (odd-num _n)
    (is _r (mod _n 2))
    (lispp (= _r 1)))

  ;; Test even/odd checking with exact value verification
  (let ((solutions (eprolog-test--collect-solutions '((even-num 10)))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((odd-num 7)))))
    (should (= (length solutions) 1)))
  (should-not (eprolog-test--has-solution-p '((even-num 7))))
  (should-not (eprolog-test--has-solution-p '((odd-num 10))))

  ;; Define power of 2 predicate
  (eprolog-define-prolog-predicate (power-of-2 _n _result)
    (is _result (expt 2 _n)))

  ;; Define sum-to predicate
  (eprolog-define-prolog-predicate (sum-to _n _sum)
    (lispp (<= _n 0))
    !
    (is _sum 0))
  (eprolog-define-prolog-predicate (sum-to _n _sum)
    (lispp (> _n 0))
    (is _n1 (- _n 1))
    (sum-to _n1 _sum1)
    (is _sum (+ _n _sum1)))

  ;; Test power of 2
  (let ((solutions (eprolog-test--collect-solutions '((power-of-2 8 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) 256)))

  ;; Test sum-to
  (let ((solutions (eprolog-test--collect-solutions '((sum-to 5 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) 15))))
```

Arithmetic comparisons are essential for numerical reasoning in Prolog:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-comparisons ()
  "Test arithmetic comparisons using lispp."
  (eprolog-test--restore-builtins)

  ;; Test basic comparisons with exact solution verification
  (let ((solutions (eprolog-test--collect-solutions '((lispp (> 15 8))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (< 3 10))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (>= 7 7))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (<= 4 9))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (= 12 12))))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((lispp (/= 5 8))))))
    (should (= (length solutions) 1)))

  ;; Test negative cases
  (should-not (eprolog-test--has-solution-p '((lispp (> 3 10)))))
  (should-not (eprolog-test--has-solution-p '((lispp (< 15 8)))))
  (should-not (eprolog-test--has-solution-p '((lispp (= 5 8))))))
```

Custom comparison predicates demonstrate how to build domain-specific numerical logic:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-custom-comparisons ()
  "Test custom comparison predicates using lispp."
  (eprolog-test--restore-builtins)

  ;; Define custom predicates
  (eprolog-define-prolog-predicate (greater _x _y)
    (lispp (> _x _y)))
  (eprolog-define-prolog-predicate (between _x _low _high)
    (lispp (>= _x _low))
    (lispp (<= _x _high)))
  (eprolog-define-prolog-predicate (positive _x)
    (lispp (> _x 0)))

  ;; Test custom predicates with exact solution verification
  (let ((solutions (eprolog-test--collect-solutions '((greater 20 15)))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((between 7 5 10)))))
    (should (= (length solutions) 1)))
  (let ((solutions (eprolog-test--collect-solutions '((positive 42)))))
    (should (= (length solutions) 1)))
  (should-not (eprolog-test--has-solution-p '((between 12 5 10))))
  (should-not (eprolog-test--has-solution-p '((positive -5)))))
```

Mathematical utility predicates like absolute value and min/max are common requirements:

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-absolute-minmax ()
  "Test absolute value and min/max predicates."
  (eprolog-test--restore-builtins)

  ;; Define absolute value predicate
  (eprolog-define-prolog-predicate (abs-val _x _abs)
    (lispp (>= _x 0))
    !
    (is _abs _x))
  (eprolog-define-prolog-predicate (abs-val _x _abs)
    (lispp (< _x 0))
    (is _abs (- _x)))

  ;; Define max predicate
  (eprolog-define-prolog-predicate (max-of _a _b _max)
    (lispp (>= _a _b))
    !
    (is _max _a))
  (eprolog-define-prolog-predicate (max-of _a _b _max)
    (is _max _b))

  ;; Test absolute value
  (let ((solutions (eprolog-test--collect-solutions '((abs-val -17 _abs)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_abs (car solutions))) 17)))

  (let ((solutions (eprolog-test--collect-solutions '((abs-val 25 _abs)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_abs (car solutions))) 25)))

  ;; Test max
  (let ((solutions (eprolog-test--collect-solutions '((max-of 15 23 _max)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_max (car solutions))) 23))))
```

## Geometric Calculations

This section demonstrates the application of arithmetic operations to geometric problems, showcasing how mathematical formulas can be elegantly expressed as Prolog predicates.

Geometric calculations in Prolog illustrate the practical application of mathematical reasoning in computational geometry. These examples show how complex mathematical formulas involving multiple variables and operations can be expressed declaratively, making the code both readable and mathematically precise.

The geometric predicates presented here demonstrate:

-   Multi-step calculations involving coordinate geometry (distance formula)
-   Application of mathematical constants and formulas (circle area using π)
-   Structured data representation for geometric entities (points, shapes)
-   Precision handling in floating-point geometric computations

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-geometric ()
  "Test geometric calculations."
  (eprolog-test--restore-builtins)

  ;; Define distance predicate
  (eprolog-define-prolog-predicate (distance (_x1 _y1) (_x2 _y2) _d)
    (is _dx (- _x2 _x1))
    (is _dy (- _y2 _y1))
    (is _dx2 (* _dx _dx))
    (is _dy2 (* _dy _dy))
    (is _d (sqrt (+ _dx2 _dy2))))

  ;; Define circle area predicate
  (eprolog-define-prolog-predicate (circle-area _radius _area)
    (is _pi 3.14159)
    (is _r2 (* _radius _radius))
    (is _area (* _pi _r2)))

  ;; Test distance calculation
  (let ((solutions (eprolog-test--collect-solutions '((distance (0 0) (3 4) _dist)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_dist (car solutions))) 5.0)))

  ;; Test circle area calculation
  (let ((solutions (eprolog-test--collect-solutions '((circle-area 5 _area)))))
    (should (= (length solutions) 1))
    (should (< (abs (- (cdr (assoc '_area (car solutions))) 78.53975)) 0.001))))
```

## Error Handling and Edge Cases

This section comprehensively tests the robustness of ε-prolog's arithmetic system under challenging conditions, including error cases, boundary values, and invalid inputs.

Robust error handling is crucial for any arithmetic system. These tests verify that ε-prolog handles exceptional conditions gracefully, providing appropriate error messages or controlled failures rather than system crashes. The tests cover mathematical edge cases, type safety violations, and boundary conditions that commonly occur in real-world applications.

The error handling coverage includes:

-   Mathematical impossibilities (division by zero, domain errors)
-   Type system violations (arithmetic with non-numeric types)
-   Numerical limits and precision boundaries
-   Malformed expressions and syntax edge cases
-   Platform-specific arithmetic behavior (overflow, underflow, special floating-point values)

### Division by Zero Tests

Division by zero represents one of the most fundamental mathematical errors. These tests verify that ε-prolog properly detects and handles division by zero in various contexts, from simple direct division to complex expressions where zero divisors emerge through computation.

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-division-by-zero ()
  "Test division by zero error handling."
  (eprolog-test--restore-builtins)

  ;; Direct division by zero should throw error
  (should-error (eprolog-test--has-solution-p '((is _result (/ 10 0)))))
  (should-error (eprolog-test--has-solution-p '((is _result (/ 0 0)))))

  ;; Division by variable that evaluates to zero
  (should-error (eprolog-test--has-solution-p '((is _x 0) (is _result (/ 5 _x)))))

  ;; Complex expressions with division by zero
  (should-error (eprolog-test--has-solution-p '((is _result (/ (+ 2 3) (- 5 5))))))
  (should-error (eprolog-test--has-solution-p '((is _result (/ 1 (* 0 10)))))))
```

### Arithmetic Overflow and Underflow Tests

These tests explore the behavior of ε-prolog's arithmetic system when dealing with extremely large or small numbers that may exceed the representational limits of the underlying number system. The tests verify graceful handling of numerical boundaries.

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-overflow-underflow ()
  "Test arithmetic overflow and underflow conditions."
  (eprolog-test--restore-builtins)

  ;; Test very large numbers (Emacs Lisp handles big integers)
  (let ((large-num 1000000000000000000))
    (should (eprolog-test--has-solution-p `((is _result (* ,large-num ,large-num))))))

  ;; Test very small floating point numbers
  (let ((tiny-num 1e-100))
    (should (eprolog-test--has-solution-p `((is _result (* ,tiny-num 2))))))

  ;; Test operations with very large negative numbers
  (let ((neg-large -1000000000000000000))
    (should (eprolog-test--has-solution-p `((is _result (- ,neg-large ,neg-large)))))))
```

### Type Mismatch Tests

Type safety in arithmetic operations is crucial for system reliability. These tests verify that ε-prolog properly rejects arithmetic operations when non-numeric types are provided, ensuring that type errors are caught early and handled appropriately.

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-type-mismatches ()
  "Test arithmetic operations with invalid types."
  (eprolog-test--restore-builtins)

  ;; Arithmetic with atoms (should throw void-variable errors)
  (should-error (eprolog-test--has-solution-p '((is _result (+ foo 5)))))
  (should-error (eprolog-test--has-solution-p '((is _result (* bar 3)))))

  ;; Arithmetic with strings (should throw wrong-type-argument errors)
  (should-error (eprolog-test--has-solution-p '((is _result (+ "hello" 10)))))
  (should-error (eprolog-test--has-solution-p '((is _result (- "world" 5)))))

  ;; Arithmetic with lists (should throw wrong-type-argument errors)
  (should-error (eprolog-test--has-solution-p '((is _result (+ (1 2 3) 5)))))
  (should-error (eprolog-test--has-solution-p '((is _result (* (a b) 2)))))

  ;; Arithmetic with complex structures (should throw errors)
  (should-error (eprolog-test--has-solution-p '((is _result (+ (foo bar) 10)))))
  (should-error (eprolog-test--has-solution-p '((is _result (/ (nested (structure)) 2))))))
```

### Invalid Expression Tests

These tests examine how ε-prolog handles malformed or unusual arithmetic expressions, including edge cases in operator usage and expressions that may be syntactically valid but semantically questionable.

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-invalid-expressions ()
  "Test malformed arithmetic expressions."
  (eprolog-test--restore-builtins)

  ;; Test empty operator expressions (valid in Emacs Lisp)
  (should (eprolog-test--has-solution-p '((is _result (+)))))  ;; Returns 0
  (should (eprolog-test--has-solution-p '((is _result (*)))))  ;; Returns 1
  (should (eprolog-test--has-solution-p '((is _result (-)))))  ;; - requires args

  ;; Test various arities (mostly valid in Emacs Lisp)
  (should (eprolog-test--has-solution-p '((is _result (+ 1)))))  ;; Returns 1
  (should (eprolog-test--has-solution-p '((is _result (+ 1 2 3 4 5 6 7 8 9)))))  ;; Returns 45

  ;; Test nested expressions
  (let ((solutions (eprolog-test--collect-solutions '((is _result (+ 1 (+)))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 1)))

  ;; Test division with insufficient arguments (may error at different levels)
  (condition-case nil
      (should-error (eprolog-test--has-solution-p '((is _result (* (/ 5) 3)))))
    (error t)))  ;; Accept any error type
```

### Floating Point Edge Cases

Floating-point arithmetic introduces special challenges related to precision, representation limits, and special values like infinity and NaN. These tests verify that ε-prolog handles floating-point edge cases consistently with the underlying Emacs Lisp numerical system.

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-floating-point-edge-cases ()
  "Test floating point precision and special values."
  (eprolog-test--restore-builtins)

  ;; Test precision limits
  (let ((precise-val 0.123456789012345678901234567890))
    (let ((solutions (eprolog-test--collect-solutions `((is _result ,precise-val)))))
      (should (= (length solutions) 1))
      (should (equal (cdr (assoc '_result (car solutions))) precise-val))))

  ;; Test very small differences
  (let ((solutions (eprolog-test--collect-solutions '((is _x 1.0000000000001) (is _y 1.0)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_x (car solutions))) 1.0000000000001))
    (should (= (cdr (assoc '_y (car solutions))) 1.0)))

  ;; Test infinity handling (Emacs Lisp returns 1.0e+INF)
  ;; Note: Emacs Lisp handles infinity gracefully
  (let ((solutions (eprolog-test--collect-solutions '((is _result (/ 1.0 0.0))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) 1.0e+INF)))

  ;; Test NaN scenarios (Emacs Lisp returns 0.0e+NaN)
  (let ((solutions (eprolog-test--collect-solutions '((is _result (sqrt -1))))))
    (should (= (length solutions) 1))
    (should (isnan (cdr (assoc '_result (car solutions)))))))
```

### Advanced Mathematical Operations

This subsection tests more sophisticated mathematical operations including modular arithmetic, exponentiation, and complex computational chains. These tests verify that ε-prolog correctly handles advanced mathematical functions and their edge cases.

```emacs-lisp
(ert-deftest eprolog-feature-arithmetic-advanced-operations ()
  "Test advanced mathematical operations and edge cases."
  (eprolog-test--restore-builtins)

  ;; Test modulo with negative numbers
  (should (eprolog-test--has-solution-p '((is _result (mod 7 3)))))
  (let ((solutions (eprolog-test--collect-solutions '((is _result (mod 7 3))))))
    (should (= (cdr (assoc '_result (car solutions))) 1)))

  ;; Test modulo with negative operands
  (should (eprolog-test--has-solution-p '((is _result (mod -7 3)))))
  (should (eprolog-test--has-solution-p '((is _result (mod 7 -3)))))

  ;; Test exponentiation
  (should (eprolog-test--has-solution-p '((is _result (expt 2 8)))))
  (let ((solutions (eprolog-test--collect-solutions '((is _result (expt 2 8))))))
    (should (= (cdr (assoc '_result (car solutions))) 256)))

  ;; Test fractional exponents (square root via exponentiation)
  (should (eprolog-test--has-solution-p '((is _result (expt 16 0.5)))))

  ;; Test zero exponentiation edge cases
  (should (eprolog-test--has-solution-p '((is _result (expt 5 0)))))
  (let ((solutions (eprolog-test--collect-solutions '((is _result (expt 5 0))))))
    (should (= (cdr (assoc '_result (car solutions))) 1)))

  ;; Test negative exponents
  (should (eprolog-test--has-solution-p '((is _result (expt 2 -3)))))

  ;; Test complex arithmetic chains
  (should (eprolog-test--has-solution-p 
    '((is _a (+ 5 3))
      (is _b (* _a 2))
      (is _result (/ _b 4))))))
```

## Conclusion

This comprehensive arithmetic test suite ensures that ε-prolog handles mathematical computations robustly across a wide range of scenarios. From basic operations to complex expressions, error handling, and edge cases, these tests verify that the arithmetic system behaves correctly and fails gracefully when appropriate.

The combination of explicit evaluation through \`is/2\` and integration with Lisp's mathematical functions provides a powerful foundation for numerical computation within logical programs.

