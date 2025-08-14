;;; eprolog-test.el --- Tests for eprolog.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Masaya Taniguchi

;; Author: Masaya Taniguchi
;; Keywords: languages, prolog, logic programming, test

;;; Commentary:

;; Test suite for eprolog.el - comprehensive tests for the exported API.

;;; Code:

(require 'ert)
(require 'eprolog)

;;; Test utilities

(defvar eprolog-test--builtin-predicates nil
  "Saved built-in predicates for test isolation.")

(defun eprolog-test--save-builtins ()
  "Save current built-in predicates (both functions and built-in Prolog predicates)."
  (setq eprolog-test--builtin-predicates
        (copy-alist eprolog-clause-database)))

(defun eprolog-test--restore-builtins ()
  "Restore built-in predicates and clear user-defined ones."
  (setq eprolog-clause-database (copy-alist eprolog-test--builtin-predicates)))

(defun eprolog-test--collect-solutions (goals)
  "Collect all solutions for GOALS as a list of binding alists."
  (let ((solutions '()))
    (eprolog-solve goals
                   :success (lambda (bindings)
                             (push bindings solutions))
                   :failure (lambda ()))
    (nreverse solutions)))

(defun eprolog-test--has-solution-p (goals)
  "Return non-nil if GOALS has at least one solution."
  (let ((found nil))
    (catch 'eprolog-test-first-solution
      (eprolog-solve goals
                     :success (lambda (_bindings) 
                               (setq found t)
                               ;; Throw to exit immediately after first solution
                               (throw 'eprolog-test-first-solution t))
                     :failure (lambda ())))
    found))

;; Save built-ins when this file loads
(eprolog-test--save-builtins)

;;; Basic predicate definition tests

(ert-deftest eprolog-test-define-fact ()
  "Test defining simple facts."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (parent tom bob))
  (eprolog-define-prolog-predicate (parent bob ann))
  
  (should (eprolog-test--has-solution-p '((parent tom bob))))
  (should (eprolog-test--has-solution-p '((parent bob ann))))
  (should-not (eprolog-test--has-solution-p '((parent ann tom)))))

(ert-deftest eprolog-test-define-rule ()
  "Test defining rules with body."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (parent tom bob))
  (eprolog-define-prolog-predicate (parent bob ann))
  (eprolog-define-prolog-predicate (grandparent _x _z)
    (parent _x _y)
    (parent _y _z))
  
  (should (eprolog-test--has-solution-p '((grandparent tom ann))))
  (should-not (eprolog-test--has-solution-p '((grandparent bob tom)))))

(ert-deftest eprolog-test-define-predicate-replacement ()
  "Test predicate replacement with !."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (test-pred a))
  (eprolog-define-prolog-predicate (test-pred b))
  (should (= (length (eprolog-test--collect-solutions '((test-pred _x)))) 2))
  
  (eprolog-define-prolog-predicate! (test-pred c))
  (let ((solutions (eprolog-test--collect-solutions '((test-pred _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 'c))))

;;; Variable and unification tests

(ert-deftest eprolog-test-variable-unification ()
  "Test basic variable unification."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (likes mary food))
  (eprolog-define-prolog-predicate (likes mary wine))
  (eprolog-define-prolog-predicate (likes john wine))
  
  (let ((solutions (eprolog-test--collect-solutions '((likes mary _x)))))
    (should (= (length solutions) 2))
    (should (member '((_x . food)) solutions))
    (should (member '((_x . wine)) solutions))))

(ert-deftest eprolog-test-anonymous-variables ()
  "Test anonymous variable handling."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (test _ _))
  (should (eprolog-test--has-solution-p '((test a b)))))

;;; Built-in predicate tests

(ert-deftest eprolog-test-unification-predicate ()
  "Test the = predicate."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((= 1 1))))
  (should-not (eprolog-test--has-solution-p '((= 1 2))))
  
  (let ((solutions (eprolog-test--collect-solutions '((= _x 42)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 42))))

(ert-deftest eprolog-test-strict-equality ()
  "Test the == predicate."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((== 1 1))))
  (should-not (eprolog-test--has-solution-p '((== 1 2))))
  (should-not (eprolog-test--has-solution-p '((== _x _y)))))

(ert-deftest eprolog-test-type-predicates ()
  "Test type checking predicates."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((atom foo))))
  (should-not (eprolog-test--has-solution-p '((atom _x))))
  (should-not (eprolog-test--has-solution-p '((atom (foo bar)))))
  
  (should (eprolog-test--has-solution-p '((var _x))))
  (should-not (eprolog-test--has-solution-p '((var foo))))
  
  (should (eprolog-test--has-solution-p '((number 42))))
  (should-not (eprolog-test--has-solution-p '((number foo))))
  
  (should (eprolog-test--has-solution-p '((string "hello"))))
  (should-not (eprolog-test--has-solution-p '((string foo)))))

(ert-deftest eprolog-test-fail-predicate ()
  "Test the fail predicate."
  (eprolog-test--restore-builtins)
  (should-not (eprolog-test--has-solution-p '((fail)))))

(ert-deftest eprolog-test-cut-predicate ()
  "Test cut (!) behavior."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate! (choice a))
  (eprolog-define-prolog-predicate (choice b))
  (eprolog-define-prolog-predicate (choice c))
  
  (eprolog-define-prolog-predicate! (test-cut _x)
    (choice _x) !)
  
  (let ((solutions (eprolog-test--collect-solutions '((test-cut _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 'a))))

;;; Control flow tests

(ert-deftest eprolog-test-call-predicate ()
  "Test the call predicate."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (likes mary food))
  
  (should (eprolog-test--has-solution-p '((call likes mary food))))
  (should (eprolog-test--has-solution-p '((call = _x 42) (= _x 42)))))

(ert-deftest eprolog-test-logical-predicates ()
  "Test logical predicates (and, or, not)."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate p)
  (eprolog-define-prolog-predicate q)
  
  (should (eprolog-test--has-solution-p '((and p q))))
  (should (eprolog-test--has-solution-p '((or p fail))))
  (should-not (eprolog-test--has-solution-p '((and p fail))))
  
  (should (eprolog-test--has-solution-p '((not fail))))
  (should-not (eprolog-test--has-solution-p '((not p)))))

(ert-deftest eprolog-test-if-then-else ()
  "Test conditional predicate (if)."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate true-pred)
  (eprolog-define-prolog-predicate then-pred)
  (eprolog-define-prolog-predicate else-pred)
  
  (should (eprolog-test--has-solution-p '((if true-pred then-pred))))
  (should (eprolog-test--has-solution-p '((if fail then-pred else-pred)))))

;;; List manipulation tests

(ert-deftest eprolog-test-member-predicate ()
  "Test list membership predicate."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((member a (a b c)))))
  (should (eprolog-test--has-solution-p '((member c (a b c)))))
  (should-not (eprolog-test--has-solution-p '((member d (a b c)))))
  
  (let ((solutions (eprolog-test--collect-solutions '((member _x (1 2 3))))))
    (should (= (length solutions) 3))))

(ert-deftest eprolog-test-append-predicate ()
  "Test list concatenation predicate."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((append (a b) (c d) (a b c d)))))
  (should-not (eprolog-test--has-solution-p '((append (a b) (c d) (a b c)))))
  
  (let ((solutions (eprolog-test--collect-solutions '((append _x _y (a b c))))))
    (should (= (length solutions) 4))))

(ert-deftest eprolog-test-maplist-predicate ()
  "Test maplist predicate."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (succ _x _y)
    (is _y (+ _x 1)))
  
  (should (eprolog-test--has-solution-p '((maplist succ (1 2 3) (2 3 4))))))

;;; Lisp integration tests

(ert-deftest eprolog-test-lisp-predicate ()
  "Test Lisp evaluation predicate."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((lisp _x (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 5))))

(ert-deftest eprolog-test-lisp-side-effects ()
  "Test Lisp side effect predicate."
  (eprolog-test--restore-builtins)
  (setq eprolog-test--temp-var nil)
  (should (eprolog-test--has-solution-p `((lisp! (setq eprolog-test--temp-var 'modified)))))
  (should (eq eprolog-test--temp-var 'modified)))

(ert-deftest eprolog-test-lisp-conditional ()
  "Test Lisp conditional predicate."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((lispp (> 5 3)))))
  (should-not (eprolog-test--has-solution-p '((lispp (> 3 5))))))

;;; Dynamic parameter tests

(ert-deftest eprolog-test-dynamic-parameters ()
  "Test dynamic parameter storage and retrieval."
  (eprolog-test--restore-builtins)
  (let ((eprolog-dynamic-parameters '()))
    (let ((solutions (eprolog-test--collect-solutions '((dynamic-put test-key 42) (dynamic-get test-key _value)))))
      (should (= (length solutions) 1))
      (should (equal (cdaar solutions) 42)))))

;;; Arithmetic tests

(ert-deftest eprolog-test-is-predicate ()
  "Test arithmetic evaluation with is predicate."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 5))))

;;; DCG (Grammar) tests

(ert-deftest eprolog-test-dcg-basic ()
  "Test basic DCG functionality."
  (eprolog-test--restore-builtins)
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar noun "dog")  ; Use without ! to add second clause
  (eprolog-define-grammar! verb "runs")
  (eprolog-define-grammar! sentence noun verb)
  
  (should (eprolog-test--has-solution-p '((phrase sentence ("cat" "runs")))))
  (should (eprolog-test--has-solution-p '((phrase sentence ("dog" "runs")))))
  (should-not (eprolog-test--has-solution-p '((phrase sentence ("cat" "sleeps"))))))

(ert-deftest eprolog-test-dcg-with-args ()
  "Test DCG with arguments."
  (eprolog-test--restore-builtins)
  (eprolog-define-grammar! (noun singular) "cat")
  (eprolog-define-grammar (noun plural) "cats")  ; Use without ! for different arity
  
  (should (eprolog-test--has-solution-p '((phrase (noun singular) ("cat")))))
  (should (eprolog-test--has-solution-p '((phrase (noun plural) ("cats")))))
  (should-not (eprolog-test--has-solution-p '((phrase (noun singular) ("cats"))))))

(ert-deftest eprolog-test-dcg-semantic-actions ()
  "Test DCG with semantic actions."
  (eprolog-test--restore-builtins)
  (eprolog-define-grammar! (digit _n) "1" (@ (= _n 1)))
  (eprolog-define-grammar (digit _n) "2" (@ (= _n 2)))  ; Use without ! to add second clause
  
  (let ((solutions (eprolog-test--collect-solutions '((phrase (digit _n) ("1"))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 1))))

;;; Error handling and edge cases

(ert-deftest eprolog-test-occurs-check ()
  "Test occurs check in unification."
  (eprolog-test--restore-builtins)
  (let ((eprolog-occurs-check t))
    (should-not (eprolog-test--has-solution-p '((= _x (_x)))))))

(ert-deftest eprolog-test-ground-predicate ()
  "Test ground term checking."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((ground 42))))
  (should (eprolog-test--has-solution-p '((ground (a b c)))))
  (should-not (eprolog-test--has-solution-p '((ground _x)))))

;;; Complex integration tests

(ert-deftest eprolog-test-factorial ()
  "Test recursive factorial predicate."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate! (factorial 0 1))
  (eprolog-define-prolog-predicate (factorial _n _f)
    (lispp (> _n 0))
    (is _n1 (- _n 1))
    (factorial _n1 _f1)
    (is _f (* _n _f1)))
  
  (let ((solutions (eprolog-test--collect-solutions '((factorial 4 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 24))))

(ert-deftest eprolog-test-backtracking-with-cut ()
  "Test complex backtracking with cut."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate! (color red))
  (eprolog-define-prolog-predicate (color green))
  (eprolog-define-prolog-predicate (color blue))
  
  (eprolog-define-prolog-predicate! (first-color _x)
    (color _x) !)
  
  (let ((solutions (eprolog-test--collect-solutions '((first-color _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 'red))))

(ert-deftest eprolog-test-repeat-predicate ()
  "Test repeat predicate with cut."
  (eprolog-test--restore-builtins)
  (setq eprolog-test--counter 0)
  (eprolog-define-prolog-predicate (test-repeat)
    (repeat)
    (lisp! (setq eprolog-test--counter (1+ eprolog-test--counter)))
    (lispp (>= eprolog-test--counter 3))
    !)
  
  (should (eprolog-test--has-solution-p '((test-repeat))))
  (should (= eprolog-test--counter 3)))

;;; Performance and stress tests

(ert-deftest eprolog-test-large-database ()
  "Test performance with larger clause database."
  (eprolog-test--restore-builtins)
  (dotimes (i 100)
    (eval `(eprolog-define-prolog-predicate (test-num ,i))))
  
  (let ((solutions (eprolog-test--collect-solutions '((test-num _x)))))
    (should (= (length solutions) 100))))

;;; Arithmetic tests with is/2 and lispp

(ert-deftest eprolog-test-basic-arithmetic-operations ()
  "Test basic arithmetic operations with is/2."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ 15 25))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 40)))
  
  (let ((solutions (eprolog-test--collect-solutions '((is _x (* 12 8))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 96)))
  
  (let ((solutions (eprolog-test--collect-solutions '((is _x (- 100 37))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 63))))

(ert-deftest eprolog-test-complex-arithmetic ()
  "Test complex arithmetic expressions."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ (* 2 3) (/ 8 2)))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 10)))
  
  (let ((solutions (eprolog-test--collect-solutions '((is _x (sqrt 16))))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 4.0))))

(ert-deftest eprolog-test-arithmetic-comparisons-with-lispp ()
  "Test arithmetic comparisons using lispp."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((lispp (> 15 8)))))
  (should (eprolog-test--has-solution-p '((lispp (< 3 10)))))
  (should (eprolog-test--has-solution-p '((lispp (>= 7 7)))))
  (should (eprolog-test--has-solution-p '((lispp (<= 4 9)))))
  (should (eprolog-test--has-solution-p '((lispp (= 12 12)))))
  (should (eprolog-test--has-solution-p '((lispp (/= 5 8)))))
  
  (should-not (eprolog-test--has-solution-p '((lispp (> 3 10)))))
  (should-not (eprolog-test--has-solution-p '((lispp (< 15 8)))))
  (should-not (eprolog-test--has-solution-p '((lispp (= 5 8))))))

(ert-deftest eprolog-test-custom-comparison-predicates ()
  "Test custom comparison predicates using lispp."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (greater _x _y)
    (lispp (> _x _y)))
  (eprolog-define-prolog-predicate (between _x _low _high)
    (lispp (>= _x _low))
    (lispp (<= _x _high)))
  (eprolog-define-prolog-predicate (positive _x)
    (lispp (> _x 0)))
  
  (should (eprolog-test--has-solution-p '((greater 20 15))))
  (should (eprolog-test--has-solution-p '((between 7 5 10))))
  (should (eprolog-test--has-solution-p '((positive 42))))
  (should-not (eprolog-test--has-solution-p '((between 12 5 10))))
  (should-not (eprolog-test--has-solution-p '((positive -5)))))

(ert-deftest eprolog-test-fibonacci-with-lispp ()
  "Test Fibonacci sequence using is/2 and lispp."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate! (fib 0 0))
  (eprolog-define-prolog-predicate (fib 1 1))
  (eprolog-define-prolog-predicate (fib _n _f)
    (lispp (> _n 1))
    (is _n1 (- _n 1))
    (is _n2 (- _n 2))
    (fib _n1 _f1)
    (fib _n2 _f2)
    (is _f (+ _f1 _f2)))
  
  (let ((solutions (eprolog-test--collect-solutions '((fib 3 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 2))))

(ert-deftest eprolog-test-gcd-algorithm ()
  "Test Greatest Common Divisor using Euclidean algorithm."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate! (gcd _a 0 _a))
  (eprolog-define-prolog-predicate (gcd _a _b _g)
    (lispp (> _b 0))
    (is _r (mod _a _b))
    (gcd _b _r _g))
  
  (let ((solutions (eprolog-test--collect-solutions '((gcd 48 18 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 6))))

(ert-deftest eprolog-test-geometric-calculations ()
  "Test geometric calculations with is/2."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (distance (_x1 _y1) (_x2 _y2) _d)
    (is _dx (- _x2 _x1))
    (is _dy (- _y2 _y1))
    (is _dx2 (* _dx _dx))
    (is _dy2 (* _dy _dy))
    (is _d (sqrt (+ _dx2 _dy2))))
  
  (eprolog-define-prolog-predicate (circle-area _radius _area)
    (is _pi 3.14159)
    (is _r2 (* _radius _radius))
    (is _area (* _pi _r2)))
  
  (let ((solutions (eprolog-test--collect-solutions '((distance (0 0) (3 4) _dist)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 5.0)))
  
  (let ((solutions (eprolog-test--collect-solutions '((circle-area 5 _area)))))
    (should (= (length solutions) 1))
    (should (< (abs (- (cdaar solutions) 78.53975)) 0.001))))

(ert-deftest eprolog-test-even-odd-predicates ()
  "Test even and odd number checking with lispp."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (even-num _n)
    (is _r (mod _n 2))
    (lispp (= _r 0)))
  (eprolog-define-prolog-predicate (odd-num _n)
    (is _r (mod _n 2))
    (lispp (= _r 1)))
  
  (should (eprolog-test--has-solution-p '((even-num 10))))
  (should (eprolog-test--has-solution-p '((odd-num 7))))
  (should-not (eprolog-test--has-solution-p '((even-num 7))))
  (should-not (eprolog-test--has-solution-p '((odd-num 10)))))

(ert-deftest eprolog-test-mathematical-functions ()
  "Test mathematical functions using is/2."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (power-of-2 _n _result)
    (is _result (expt 2 _n)))
  (eprolog-define-prolog-predicate (sum-to _n _sum)
    (lispp (<= _n 0))
    !
    (is _sum 0))
  (eprolog-define-prolog-predicate (sum-to _n _sum)
    (lispp (> _n 0))
    (is _n1 (- _n 1))
    (sum-to _n1 _sum1)
    (is _sum (+ _n _sum1)))
  
  (let ((solutions (eprolog-test--collect-solutions '((power-of-2 8 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 256)))
  
  (let ((solutions (eprolog-test--collect-solutions '((sum-to 5 _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 15))))

(ert-deftest eprolog-test-absolute-value-and-minmax ()
  "Test absolute value and min/max predicates."
  (eprolog-test--restore-builtins)
  (eprolog-define-prolog-predicate (abs-val _x _abs)
    (lispp (>= _x 0))
    !
    (is _abs _x))
  (eprolog-define-prolog-predicate (abs-val _x _abs)
    (lispp (< _x 0))
    (is _abs (- _x)))
  
  (eprolog-define-prolog-predicate (max-of _a _b _max)
    (lispp (>= _a _b))
    !
    (is _max _a))
  (eprolog-define-prolog-predicate (max-of _a _b _max)
    (is _max _b))
  
  (let ((solutions (eprolog-test--collect-solutions '((abs-val -17 _abs)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 17)))
  
  (let ((solutions (eprolog-test--collect-solutions '((max-of 15 23 _max)))))
    (should (= (length solutions) 1))
    (should (equal (cdaar solutions) 23))))

;;; Family tree tests (Sazae-san characters)

(ert-deftest eprolog-test-family-tree-setup ()
  "Test family tree setup and basic parent relationships."
  (eprolog-test--restore-builtins)
  
  ;; Define parent relationships
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent ikura taiko))
  (eprolog-define-prolog-predicate (parent _x _y) (parent _x _z) ! (married _y _z))

  ;; Define marriage relationships
  (eprolog-define-prolog-predicate! (married _x _y) (married-fact _x _y))
  (eprolog-define-prolog-predicate (married _x _y) (married-fact _y _x))

  (eprolog-define-prolog-predicate! (married-fact fune namihei))
  (eprolog-define-prolog-predicate (married-fact sazae masuo))
  (eprolog-define-prolog-predicate (married-fact taiko norisuke))

  ;; Define gender
  (eprolog-define-prolog-predicate! (male namihei))
  (eprolog-define-prolog-predicate (male katsuo))
  (eprolog-define-prolog-predicate (male masuo))
  (eprolog-define-prolog-predicate (male tarao))
  (eprolog-define-prolog-predicate (male norisuke))

  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female taiko))
  (eprolog-define-prolog-predicate (female wakame))

  ;; Test basic parent relationships
  (should (eprolog-test--has-solution-p '((parent katsuo fune))))
  (should (eprolog-test--has-solution-p '((parent wakame fune))))
  (should (eprolog-test--has-solution-p '((parent sazae fune))))
  (should (eprolog-test--has-solution-p '((parent tarao sazae))))
  (should (eprolog-test--has-solution-p '((parent ikura taiko))))
  
  ;; Test marriage relationships
  (should (eprolog-test--has-solution-p '((married fune namihei))))
  (should (eprolog-test--has-solution-p '((married namihei fune))))
  (should (eprolog-test--has-solution-p '((married sazae masuo))))
  (should (eprolog-test--has-solution-p '((married masuo sazae)))))

(ert-deftest eprolog-test-family-tree-complex-parent ()
  "Test complex parent relationship with marriage cut."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent ikura taiko))
  (eprolog-define-prolog-predicate (parent _x _y) (parent _x _z) ! (married _y _z))

  (eprolog-define-prolog-predicate! (married _x _y) (married-fact _x _y))
  (eprolog-define-prolog-predicate (married _x _y) (married-fact _y _x))

  (eprolog-define-prolog-predicate! (married-fact fune namihei))
  (eprolog-define-prolog-predicate (married-fact sazae masuo))
  (eprolog-define-prolog-predicate (married-fact taiko norisuke))

  ;; Test complex parent relationships (through marriage)
  (should (eprolog-test--has-solution-p '((parent katsuo namihei))))
  (should (eprolog-test--has-solution-p '((parent wakame namihei))))
  (should (eprolog-test--has-solution-p '((parent sazae namihei))))
  (should (eprolog-test--has-solution-p '((parent tarao masuo))))
  (should (eprolog-test--has-solution-p '((parent ikura norisuke)))))

(ert-deftest eprolog-test-family-tree-grandparent ()
  "Test grandparent relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree (without complex parent rule to avoid infinite recursion)
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent ikura taiko))

  (eprolog-define-prolog-predicate! (grandparent _x _z) (parent _x _y) (parent _y _z))

  ;; Test grandparent relationships
  (should (eprolog-test--has-solution-p '((grandparent tarao fune))))
  (should-not (eprolog-test--has-solution-p '((grandparent katsuo tarao)))))

(ert-deftest eprolog-test-family-tree-sibling ()
  "Test sibling relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent _x _y) (parent _x _z) ! (married _y _z))

  (eprolog-define-prolog-predicate! (married _x _y) (married-fact _x _y))
  (eprolog-define-prolog-predicate (married _x _y) (married-fact _y _x))

  (eprolog-define-prolog-predicate! (married-fact fune namihei))
  (eprolog-define-prolog-predicate (married-fact sazae masuo))

  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))

  ;; Test sibling relationships
  (should (eprolog-test--has-solution-p '((sibling katsuo wakame))))
  (should (eprolog-test--has-solution-p '((sibling wakame sazae))))
  (should (eprolog-test--has-solution-p '((sibling katsuo sazae))))
  (should-not (eprolog-test--has-solution-p '((sibling katsuo katsuo)))))

(ert-deftest eprolog-test-family-tree-ancestor ()
  "Test ancestor relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree (without complex parent rule)
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))

  (eprolog-define-prolog-predicate! (ancestor _x _y) (parent _x _y))
  (eprolog-define-prolog-predicate (ancestor _x _y) (parent _x _z) (ancestor _z _y))

  ;; Test ancestor relationships
  (should (eprolog-test--has-solution-p '((ancestor tarao sazae))))
  (should (eprolog-test--has-solution-p '((ancestor tarao fune))))
  (should-not (eprolog-test--has-solution-p '((ancestor sazae tarao)))))

(ert-deftest eprolog-test-family-tree-child ()
  "Test child relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))

  (eprolog-define-prolog-predicate! (child _x _y) (parent _y _x))

  ;; Test child relationships
  (should (eprolog-test--has-solution-p '((child fune katsuo))))
  (should (eprolog-test--has-solution-p '((child fune wakame))))
  (should (eprolog-test--has-solution-p '((child fune sazae))))
  (should (eprolog-test--has-solution-p '((child sazae tarao))))
  (should-not (eprolog-test--has-solution-p '((child katsuo fune)))))

(ert-deftest eprolog-test-family-tree-mother-father ()
  "Test mother and father relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent _x _y) (parent _x _z) ! (married _y _z))

  (eprolog-define-prolog-predicate! (married _x _y) (married-fact _x _y))
  (eprolog-define-prolog-predicate (married _x _y) (married-fact _y _x))

  (eprolog-define-prolog-predicate! (married-fact fune namihei))
  (eprolog-define-prolog-predicate (married-fact sazae masuo))

  (eprolog-define-prolog-predicate! (male namihei))
  (eprolog-define-prolog-predicate (male katsuo))
  (eprolog-define-prolog-predicate (male masuo))
  (eprolog-define-prolog-predicate (male tarao))

  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female wakame))

  (eprolog-define-prolog-predicate! (mother _x _y) (parent _x _y) (female _y))
  (eprolog-define-prolog-predicate! (father _x _y) (parent _x _y) (male _y))

  ;; Test mother relationships
  (should (eprolog-test--has-solution-p '((mother katsuo fune))))
  (should (eprolog-test--has-solution-p '((mother wakame fune))))
  (should (eprolog-test--has-solution-p '((mother sazae fune))))
  (should (eprolog-test--has-solution-p '((mother tarao sazae))))

  ;; Test father relationships
  (should (eprolog-test--has-solution-p '((father katsuo namihei))))
  (should (eprolog-test--has-solution-p '((father wakame namihei))))
  (should (eprolog-test--has-solution-p '((father sazae namihei))))
  (should (eprolog-test--has-solution-p '((father tarao masuo)))))

(ert-deftest eprolog-test-family-tree-sister-brother ()
  "Test sister and brother relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))

  (eprolog-define-prolog-predicate! (male katsuo))
  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female wakame))

  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))
  (eprolog-define-prolog-predicate! (sister _x _y) (sibling _x _y) (female _y))
  (eprolog-define-prolog-predicate! (brother _x _y) (sibling _x _y) (male _y))

  ;; Test sister relationships
  (should (eprolog-test--has-solution-p '((sister katsuo wakame))))
  (should (eprolog-test--has-solution-p '((sister katsuo sazae))))
  (should (eprolog-test--has-solution-p '((sister wakame sazae))))

  ;; Test brother relationships
  (should (eprolog-test--has-solution-p '((brother wakame katsuo))))
  (should (eprolog-test--has-solution-p '((brother sazae katsuo))))
  (should-not (eprolog-test--has-solution-p '((brother wakame sazae)))))

(ert-deftest eprolog-test-family-tree-uncle-aunt ()
  "Test uncle and aunt relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))

  (eprolog-define-prolog-predicate! (male katsuo))
  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female wakame))

  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))
  (eprolog-define-prolog-predicate! (uncle _x _y) (parent _y _z) (sibling _x _z) (male _x))
  (eprolog-define-prolog-predicate! (aunt _x _y) (parent _y _z) (sibling _x _z) (female _x))

  ;; Test uncle relationships
  (should (eprolog-test--has-solution-p '((uncle katsuo tarao))))

  ;; Test aunt relationships
  (should (eprolog-test--has-solution-p '((aunt wakame tarao)))))

(ert-deftest eprolog-test-family-tree-cousin ()
  "Test cousin relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree with cousins
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  
  ;; Add another generation to create cousins
  (eprolog-define-prolog-predicate (parent child1 katsuo))
  (eprolog-define-prolog-predicate (parent child2 wakame))

  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))
  (eprolog-define-prolog-predicate! (cousin _x _y) (parent _x _a) (parent _y _b) (sibling _a _b))

  ;; Test cousin relationships
  (should (eprolog-test--has-solution-p '((cousin tarao child1))))
  (should (eprolog-test--has-solution-p '((cousin tarao child2))))
  (should (eprolog-test--has-solution-p '((cousin child1 tarao))))
  (should (eprolog-test--has-solution-p '((cousin child1 child2)))))

(ert-deftest eprolog-test-family-tree-complex-queries ()
  "Test complex family tree queries."
  (eprolog-test--restore-builtins)
  
  ;; Setup complete family tree (without recursive parent rule)
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent ikura taiko))

  (eprolog-define-prolog-predicate! (married _x _y) (married-fact _x _y))
  (eprolog-define-prolog-predicate (married _x _y) (married-fact _y _x))

  (eprolog-define-prolog-predicate! (married-fact fune namihei))
  (eprolog-define-prolog-predicate (married-fact sazae masuo))
  (eprolog-define-prolog-predicate (married-fact taiko norisuke))

  (eprolog-define-prolog-predicate! (male namihei))
  (eprolog-define-prolog-predicate (male katsuo))
  (eprolog-define-prolog-predicate (male masuo))
  (eprolog-define-prolog-predicate (male tarao))
  (eprolog-define-prolog-predicate (male norisuke))

  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female taiko))
  (eprolog-define-prolog-predicate (female wakame))

  (eprolog-define-prolog-predicate! (grandparent _x _z) (parent _x _y) (parent _y _z))
  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))

  ;; Test finding all children of fune
  (let ((solutions (eprolog-test--collect-solutions '((parent _child fune)))))
    (should (= (length solutions) 3))
    (should (member '((_child . katsuo)) solutions))
    (should (member '((_child . wakame)) solutions))
    (should (member '((_child . sazae)) solutions)))

  ;; Test finding all grandchildren of fune
  (let ((solutions (eprolog-test--collect-solutions '((grandparent _grandchild fune)))))
    (should (= (length solutions) 1))
    (should (member '((_grandchild . tarao)) solutions)))

  ;; Test finding all siblings of katsuo
  (let ((solutions (eprolog-test--collect-solutions '((sibling katsuo _sibling)))))
    (should (= (length solutions) 2))
    (should (member '((_sibling . wakame)) solutions))
    (should (member '((_sibling . sazae)) solutions))))

(provide 'eprolog-test)

;;; eprolog-test.el ends here

