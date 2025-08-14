(require 'eprolog)
(require 'ert)

;; Helper function to test query success
(defun eprolog-test--has-solution-p (goals)
  "Test if GOALS has at least one solution."
  (let ((found-solution nil))
    (eprolog-solve goals 
      :success (lambda (_) (setq found-solution t)))
    found-solution))

;; Helper function to collect all solutions
(defun eprolog-test--collect-solutions (goals)
  "Collect all solutions for GOALS."
  (let ((solutions '()))
    (eprolog-solve goals
      :success (lambda (solution) 
                 (push solution solutions)))
    (nreverse solutions)))

;; Store built-in predicates for restoration
(defvar eprolog-usage--builtin-predicates
  (when (boundp 'eprolog-clause-database)
    (copy-alist eprolog-clause-database))
  "Saved copy of built-in predicates for test restoration.")

;; Helper function to restore builtins (same as existing tests)
(defun eprolog-test--restore-builtins ()
  "Restore built-in predicates and clear user-defined ones."
  (setq eprolog-clause-database (copy-alist eprolog-usage--builtin-predicates)))

(ert-deftest eprolog-usage-basic-facts-and-rules ()
  "Test basic fact and rule definition from README.org examples."
  (eprolog-test--restore-builtins)
  
  ;; Define facts using convenient alias
  (eprolog-define-predicate (parent tom bob))
  (eprolog-define-predicate (parent tom liz))
  (eprolog-define-predicate (parent bob ann))
  (eprolog-define-predicate (parent bob pat))
  (eprolog-define-predicate (parent pat jim))
  
  ;; Define rule
  (eprolog-define-predicate (grandparent _x _z)
    (parent _x _y)
    (parent _y _z))
  
  ;; Test basic facts
  (should (eprolog-test--has-solution-p '((parent tom bob))))
  (should (eprolog-test--has-solution-p '((parent pat jim))))
  (should-not (eprolog-test--has-solution-p '((parent jim tom))))
  
  ;; Test rule - grandparent relationships
  (should (eprolog-test--has-solution-p '((grandparent tom ann))))
  (should (eprolog-test--has-solution-p '((grandparent tom pat))))
  (should (eprolog-test--has-solution-p '((grandparent bob jim))))
  (should-not (eprolog-test--has-solution-p '((grandparent ann tom))))
  
  ;; Test solution collection
  (let ((solutions (eprolog-test--collect-solutions '((grandparent tom _x)))))
    (should (= (length solutions) 2))
    (should (member 'ann (mapcar (lambda (s) (cdr (assoc '_x s))) solutions)))
    (should (member 'pat (mapcar (lambda (s) (cdr (assoc '_x s))) solutions)))))

(ert-deftest eprolog-usage-predicate-definition-tests ()
  "Test basic predicate definition functionality."
  (eprolog-test--restore-builtins)
  
  ;; Test defining simple facts
  (eprolog-define-predicate (parent tom bob))
  (eprolog-define-predicate (parent bob ann))
  
  (should (eprolog-test--has-solution-p '((parent tom bob))))
  (should (eprolog-test--has-solution-p '((parent bob ann))))
  (should-not (eprolog-test--has-solution-p '((parent ann tom)))))

(ert-deftest eprolog-usage-predicate-replacement ()
  "Test predicate replacement with ! operator."
  (eprolog-test--restore-builtins)
  
  ;; Define multiple clauses for same predicate
  (eprolog-define-predicate (test-pred a))
  (eprolog-define-predicate (test-pred b))
  (should (= (length (eprolog-test--collect-solutions '((test-pred _x)))) 2))
  
  ;; Replace with ! - should clear existing clauses
  (eprolog-define-predicate! (test-pred c))
  (let ((solutions (eprolog-test--collect-solutions '((test-pred _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 'c))))

(ert-deftest eprolog-usage-family-tree-sazae-san ()
  "Test family tree relationships with Sazae-san characters."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  
  (eprolog-define-prolog-predicate! (grandparent _x _z)
    (parent _x _y)
    (parent _y _z))
  
  ;; Test basic parent relationships
  (should (eprolog-test--has-solution-p '((parent katsuo fune))))
  (should (eprolog-test--has-solution-p '((parent tarao sazae))))
  
  ;; Test grandparent relationship
  (should (eprolog-test--has-solution-p '((grandparent tarao fune)))))

(ert-deftest eprolog-usage-unification-and-equality ()
  "Test unification and equality predicates."
  (eprolog-test--restore-builtins)
  
  ;; Test =/2 (unification)
  (should (eprolog-test--has-solution-p '((= foo foo))))
  (should (eprolog-test--has-solution-p '((= _x bar) (= _x bar))))
  (should-not (eprolog-test--has-solution-p '((= foo bar))))
  
  ;; Test ==/2 (strict equality)
  (should (eprolog-test--has-solution-p '((== foo foo))))
  (should-not (eprolog-test--has-solution-p '((== _x foo)))))

(ert-deftest eprolog-usage-variable-unification-advanced ()
  "Test advanced variable unification patterns."
  (eprolog-test--restore-builtins)
  
  ;; Define test predicates
  (eprolog-define-predicate (likes mary food))
  (eprolog-define-predicate (likes mary wine))
  (eprolog-define-predicate (likes john wine))
  
  ;; Test multiple solutions with same variable
  (let ((solutions (eprolog-test--collect-solutions '((likes mary _x)))))
    (should (= (length solutions) 2))
    (should (member '((_x . food)) solutions))
    (should (member '((_x . wine)) solutions)))
  
  ;; Test unification with complex terms
  (let ((solutions (eprolog-test--collect-solutions '((= _x 42)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 42))))

(ert-deftest eprolog-usage-anonymous-variables ()
  "Test anonymous variable handling."
  (eprolog-test--restore-builtins)
  
  ;; Test anonymous variables don't unify with each other
  (eprolog-define-predicate (test _ _))
  (should (eprolog-test--has-solution-p '((test a b))))
  (should (eprolog-test--has-solution-p '((test foo bar))))
  (should (eprolog-test--has-solution-p '((test _x _y)))))

(ert-deftest eprolog-usage-occurs-check ()
  "Test occurs check in unification."
  (eprolog-test--restore-builtins)
  
  ;; Test occurs check prevents infinite structures
  (let ((eprolog-occurs-check t))
    (should-not (eprolog-test--has-solution-p '((= _x (_x)))))
    (should-not (eprolog-test--has-solution-p '((= _x (f _x)))))))

(ert-deftest eprolog-usage-type-checking ()
  "Test type checking predicates from README.org."
  (eprolog-test--restore-builtins)
  
  ;; Test atom/1
  (should (eprolog-test--has-solution-p '((atom foo))))
  (should-not (eprolog-test--has-solution-p '((atom (a b)))))
  
  ;; Test var/1
  (should (eprolog-test--has-solution-p '((var _x))))
  (should-not (eprolog-test--has-solution-p '((var foo))))
  
  ;; Test number/1
  (should (eprolog-test--has-solution-p '((number 42))))
  (should-not (eprolog-test--has-solution-p '((number foo))))
  
  ;; Test string/1
  (should (eprolog-test--has-solution-p '((string "hello"))))
  (should-not (eprolog-test--has-solution-p '((string foo)))))

(ert-deftest eprolog-usage-ground-predicate ()
  "Test ground term checking."
  (eprolog-test--restore-builtins)
  
  ;; Test ground terms
  (should (eprolog-test--has-solution-p '((ground 42))))
  (should (eprolog-test--has-solution-p '((ground (a b c)))))
  (should (eprolog-test--has-solution-p '((ground foo))))
  
  ;; Test non-ground terms
  (should-not (eprolog-test--has-solution-p '((ground _x))))
  (should-not (eprolog-test--has-solution-p '((ground (a _x c))))))

(ert-deftest eprolog-usage-list-operations ()
  "Test list operation predicates from README.org examples."
  (eprolog-test--restore-builtins)
  
  ;; Test member/2 as shown in README
  (let ((solutions (eprolog-test--collect-solutions '((member _x (a b c))))))
    (should (= (length solutions) 3))
    (should (member 'a (mapcar (lambda (s) (cdr (assoc '_x s))) solutions)))
    (should (member 'b (mapcar (lambda (s) (cdr (assoc '_x s))) solutions)))
    (should (member 'c (mapcar (lambda (s) (cdr (assoc '_x s))) solutions))))
  
  ;; Test specific membership
  (should (eprolog-test--has-solution-p '((member a (a b c)))))
  (should (eprolog-test--has-solution-p '((member b (a b c)))))
  (should (eprolog-test--has-solution-p '((member c (a b c)))))
  (should-not (eprolog-test--has-solution-p '((member d (a b c)))))
  
  ;; Test append/3 as shown in README
  (let ((solutions (eprolog-test--collect-solutions '((append (1 2) (3 4) _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) '(1 2 3 4))))
  
  ;; Test append/3 variations
  (should (eprolog-test--has-solution-p '((append (1 2) (3 4) (1 2 3 4)))))
  (should (eprolog-test--has-solution-p '((append () (1 2 3) (1 2 3)))))
  (should (eprolog-test--has-solution-p '((append (1 2 3) () (1 2 3)))))
  
  ;; Test append/2
  (let ((solutions (eprolog-test--collect-solutions '((append ((1 2) (3 4)) _result)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_result (car solutions))) '(1 2 3 4)))))

(ert-deftest eprolog-usage-higher-order-predicates ()
  "Test maplist higher-order predicates."
  (eprolog-test--restore-builtins)
  
  ;; Define helper predicate
  (eprolog-define-predicate (succ _x _y)
    (is _y (+ _x 1)))
  
  ;; Test maplist/2
  (should (eprolog-test--has-solution-p '((maplist succ (1 2 3) (2 3 4)))))
  (should-not (eprolog-test--has-solution-p '((maplist succ (1 2 3) (2 3 5)))))
  
  ;; Test maplist/1
  (eprolog-define-predicate (positive _x) (lispp (> _x 0)))
  (should (eprolog-test--has-solution-p '((maplist positive (1 2 3)))))
  (should-not (eprolog-test--has-solution-p '((maplist positive (0 1 2))))))

(ert-deftest eprolog-usage-control-predicates ()
  "Test control predicates."
  (eprolog-test--restore-builtins)
  
  ;; Test basic control predicates
  (should-not (eprolog-test--has-solution-p '((fail))))
  (should (eprolog-test--has-solution-p '((true))))
  (should-not (eprolog-test--has-solution-p '((false))))
  
  ;; Test not/1
  (should (eprolog-test--has-solution-p '((not fail))))
  (should-not (eprolog-test--has-solution-p '((not true)))))

(ert-deftest eprolog-usage-fail-predicate ()
  "Test the fail predicate."
  (eprolog-test--restore-builtins)
  (should-not (eprolog-test--has-solution-p '((fail)))))

(ert-deftest eprolog-usage-cut-predicate ()
  "Test cut (!) behavior as standalone predicate."
  (eprolog-test--restore-builtins)
  (eprolog-define-predicate! (choice a))
  (eprolog-define-predicate (choice b))
  (eprolog-define-predicate (choice c))
  
  (eprolog-define-predicate! (test-cut _x)
    (choice _x) !)
  
  (let ((solutions (eprolog-test--collect-solutions '((test-cut _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 'a))))

(ert-deftest eprolog-usage-call-predicate ()
  "Test the call predicate."
  (eprolog-test--restore-builtins)
  (eprolog-define-predicate (likes mary food))
  
  (should (eprolog-test--has-solution-p '((call likes mary food))))
  (should (eprolog-test--has-solution-p '((call = _x 42) (= _x 42)))))

(ert-deftest eprolog-usage-logical-predicates ()
  "Test logical conjunction and disjunction predicates."
  (eprolog-test--restore-builtins)
  
  ;; Test and/0-4
  (should (eprolog-test--has-solution-p '((and))))
  (should (eprolog-test--has-solution-p '((and true))))
  (should (eprolog-test--has-solution-p '((and true true))))
  (should-not (eprolog-test--has-solution-p '((and true fail))))
  
  ;; Test or/0-4
  (should-not (eprolog-test--has-solution-p '((or))))
  (should (eprolog-test--has-solution-p '((or true))))
  (should (eprolog-test--has-solution-p '((or fail true))))
  (should-not (eprolog-test--has-solution-p '((or fail fail))))
  
  ;; Test if/2 and if/3
  (should (eprolog-test--has-solution-p '((if true true))))
  (should-not (eprolog-test--has-solution-p '((if fail true))))
  (should (eprolog-test--has-solution-p '((if true true fail))))
  (should (eprolog-test--has-solution-p '((if fail fail true)))))

(ert-deftest eprolog-usage-if-then-else ()
  "Test conditional predicate (if) as standalone test."
  (eprolog-test--restore-builtins)
  (eprolog-define-predicate true-pred)
  (eprolog-define-predicate then-pred)
  (eprolog-define-predicate else-pred)
  
  (should (eprolog-test--has-solution-p '((if true-pred then-pred))))
  (should (eprolog-test--has-solution-p '((if fail then-pred else-pred)))))

(ert-deftest eprolog-usage-metacall-predicates ()
  "Test meta-call predicates."
  (eprolog-test--restore-builtins)
  
  ;; Define test predicate
  (eprolog-define-predicate (test-pred success))
  
  ;; Test call/1
  (should (eprolog-test--has-solution-p '((call test-pred success))))
  (should (eprolog-test--has-solution-p '((call = _x 42) (= _x 42))))
  (should (eprolog-test--has-solution-p '((call = foo foo)))))

(ert-deftest eprolog-usage-cut-semantics ()
  "Test cut (!) semantics."
  (eprolog-test--restore-builtins)
  
  ;; Define choice predicates
  (eprolog-define-predicate! (choice a))
  (eprolog-define-predicate (choice b))
  (eprolog-define-predicate (choice c))
  
  ;; Test without cut
  (let ((solutions (eprolog-test--collect-solutions '((choice _x)))))
    (should (= (length solutions) 3)))
  
  ;; Define predicate with cut
  (eprolog-define-predicate! (first-choice _x)
    (choice _x) !)
  
  ;; Test with cut
  (let ((solutions (eprolog-test--collect-solutions '((first-choice _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 'a))))

(ert-deftest eprolog-usage-repeat-predicate ()
  "Test repeat predicate for infinite choice points."
  (eprolog-test--restore-builtins)
  
  ;; Test repeat with cut (should succeed once)
  (let ((counter 0))
    (eprolog-define-predicate (test-repeat-usage)
      (repeat)
      (lisp! (setq counter (1+ counter)))
      (lispp (>= counter 3))
      !)
    (should (eprolog-test--has-solution-p '((test-repeat-usage))))
    (should (= counter 3))))

(ert-deftest eprolog-usage-lisp-integration ()
  "Test Lisp integration predicates from README.org examples."
  (eprolog-test--restore-builtins)
  
  ;; Test lisp/2 as shown in README
  (let ((solutions (eprolog-test--collect-solutions '((lisp _result (+ 1 2 3))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_result (car solutions))) 6)))
  
  ;; Test lispp/1 as shown in README
  (should (eprolog-test--has-solution-p '((lispp (> 5 3)))))
  (should-not (eprolog-test--has-solution-p '((lispp (< 5 3)))))
  
  ;; Test lisp!/1 side effects
  (let ((test-var nil))
    (should (eprolog-test--has-solution-p `((lisp! (setq test-var 'success)))))
    (should (eq test-var 'success))))

(ert-deftest eprolog-usage-lisp-predicate ()
  "Test Lisp evaluation predicate as standalone test."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((lisp _x (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 5))))

(ert-deftest eprolog-usage-lisp-side-effects ()
  "Test Lisp side effect predicate as standalone test."
  (eprolog-test--restore-builtins)
  (setq eprolog-test--temp-var nil)
  (should (eprolog-test--has-solution-p `((lisp! (setq eprolog-test--temp-var 'modified)))))
  (should (eq eprolog-test--temp-var 'modified)))

(ert-deftest eprolog-usage-lisp-conditional ()
  "Test Lisp conditional predicate as standalone test."
  (eprolog-test--restore-builtins)
  (should (eprolog-test--has-solution-p '((lispp (> 5 3)))))
  (should-not (eprolog-test--has-solution-p '((lispp (> 3 5))))))

(ert-deftest eprolog-usage-dynamic-parameters ()
  "Test dynamic parameter predicates."
  (eprolog-test--restore-builtins)
  
  ;; Test dynamic-put and dynamic-get
  (should (eprolog-test--has-solution-p 
           '((dynamic-put test-key 42)
             (dynamic-get test-key _value)
             (= _value 42))))
  
  ;; Test parameter persistence across goals
  (should (eprolog-test--has-solution-p
           '((dynamic-put counter 0)
             (dynamic-get counter _old)
             (is _new (+ _old 1))
             (dynamic-put counter _new)
             (dynamic-get counter 1)))))

(ert-deftest eprolog-usage-arithmetic ()
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
    (should (equal (cdr (assoc '_x (car solutions))) 10))))

(ert-deftest eprolog-usage-is-predicate ()
  "Test basic is/2 predicate as standalone test."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ 2 3))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 5))))

(ert-deftest eprolog-usage-basic-arithmetic-operations ()
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

(ert-deftest eprolog-usage-complex-arithmetic ()
  "Test complex arithmetic expressions."
  (eprolog-test--restore-builtins)
  (let ((solutions (eprolog-test--collect-solutions '((is _x (+ (* 2 3) (/ 8 2)))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 10)))
  
  (let ((solutions (eprolog-test--collect-solutions '((is _x (sqrt 16))))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 4.0))))

(ert-deftest eprolog-usage-mathematical-predicates ()
  "Test mathematical predicates."
  (eprolog-test--restore-builtins)
  
  ;; Define even and odd predicates
  (eprolog-define-prolog-predicate (even-num _n)
    (is _r (mod _n 2))
    (lispp (= _r 0)))
  (eprolog-define-prolog-predicate (odd-num _n)
    (is _r (mod _n 2))
    (lispp (= _r 1)))
  
  ;; Test even/odd checking
  (should (eprolog-test--has-solution-p '((even-num 10))))
  (should (eprolog-test--has-solution-p '((odd-num 7))))
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

(ert-deftest eprolog-usage-arithmetic-comparisons ()
  "Test arithmetic comparisons using lispp."
  (eprolog-test--restore-builtins)
  
  ;; Test basic comparisons
  (should (eprolog-test--has-solution-p '((lispp (> 15 8)))))
  (should (eprolog-test--has-solution-p '((lispp (< 3 10)))))
  (should (eprolog-test--has-solution-p '((lispp (>= 7 7)))))
  (should (eprolog-test--has-solution-p '((lispp (<= 4 9)))))
  (should (eprolog-test--has-solution-p '((lispp (= 12 12)))))
  (should (eprolog-test--has-solution-p '((lispp (/= 5 8)))))
  
  ;; Test negative cases
  (should-not (eprolog-test--has-solution-p '((lispp (> 3 10)))))
  (should-not (eprolog-test--has-solution-p '((lispp (< 15 8)))))
  (should-not (eprolog-test--has-solution-p '((lispp (= 5 8))))))

(ert-deftest eprolog-usage-custom-comparison-predicates ()
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
  
  ;; Test custom predicates
  (should (eprolog-test--has-solution-p '((greater 20 15))))
  (should (eprolog-test--has-solution-p '((between 7 5 10))))
  (should (eprolog-test--has-solution-p '((positive 42))))
  (should-not (eprolog-test--has-solution-p '((between 12 5 10))))
  (should-not (eprolog-test--has-solution-p '((positive -5)))))

(ert-deftest eprolog-usage-absolute-value-and-minmax ()
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

(ert-deftest eprolog-usage-geometric-calculations ()
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

(ert-deftest eprolog-usage-dcg-basic ()
  "Test basic DCG functionality."
  (eprolog-test--restore-builtins)
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar noun "dog")  ; Use without ! to add second clause
  (eprolog-define-grammar! verb "runs")
  (eprolog-define-grammar! sentence noun verb)
  
  (should (eprolog-test--has-solution-p '((phrase sentence ("cat" "runs")))))
  (should (eprolog-test--has-solution-p '((phrase sentence ("dog" "runs")))))
  (should-not (eprolog-test--has-solution-p '((phrase sentence ("cat" "sleeps"))))))

(ert-deftest eprolog-usage-dcg-basic-grammar ()
  "Test basic DCG grammar definition and parsing."
  (eprolog-test--restore-builtins)
  
  ;; Define grammar
  (eprolog-define-grammar! s np vp)
  (eprolog-define-grammar! np det noun)
  (eprolog-define-grammar! vp verb np)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar det "a")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar noun "dog")
  (eprolog-define-grammar! verb "chases")
  (eprolog-define-grammar verb "sees")
  
  ;; Test parsing valid sentences
  (should (eprolog-test--has-solution-p '((phrase s ("the" "cat" "chases" "a" "dog")))))
  (should (eprolog-test--has-solution-p '((phrase s ("a" "dog" "sees" "the" "cat")))))
  
  ;; Test parsing invalid sentences
  (should-not (eprolog-test--has-solution-p '((phrase s ("cat" "the" "chases"))))))

(ert-deftest eprolog-usage-dcg-parsing-with-remainder ()
  "Test DCG parsing with remainder."
  (eprolog-test--restore-builtins)
  
  ;; Define grammar  
  (eprolog-define-grammar! s np vp)
  (eprolog-define-grammar! np det noun)
  (eprolog-define-grammar! vp verb np)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar! verb "chases")
  
  ;; Test parsing with remainder
  (let ((solutions (eprolog-test--collect-solutions 
                    '((phrase s ("the" "cat" "chases" "the" "cat" "quickly") _rest)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_rest (car solutions))) '("quickly")))))

(ert-deftest eprolog-usage-dcg-epsilon-productions ()
  "Test DCG epsilon (empty) productions."
  (eprolog-test--restore-builtins)
  
  ;; Test epsilon productions
  (eprolog-define-grammar! optional-adj nil)
  (eprolog-define-grammar optional-adj adj)
  (eprolog-define-grammar! np det optional-adj noun)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar! adj "big")
  (eprolog-define-grammar! noun "cat")
  
  (should (eprolog-test--has-solution-p '((phrase np ("the" "cat")))))
  (should (eprolog-test--has-solution-p '((phrase np ("the" "big" "cat"))))))

(ert-deftest eprolog-usage-dcg-with-args ()
  "Test DCG with arguments."
  (eprolog-test--restore-builtins)
  
  ;; Test DCG rules with arguments for grammatical agreement
  (eprolog-define-grammar! (noun singular) "cat")
  (eprolog-define-grammar (noun plural) "cats")
  (eprolog-define-grammar! (det singular) "a")
  (eprolog-define-grammar (det plural) "some")
  
  ;; Test singular and plural agreement
  (should (eprolog-test--has-solution-p '((phrase (noun singular) ("cat")))))
  (should (eprolog-test--has-solution-p '((phrase (noun plural) ("cats")))))
  (should (eprolog-test--has-solution-p '((phrase (det singular) ("a")))))
  (should (eprolog-test--has-solution-p '((phrase (det plural) ("some")))))
  
  ;; Test mismatched agreement
  (should-not (eprolog-test--has-solution-p '((phrase (noun singular) ("cats")))))
  (should-not (eprolog-test--has-solution-p '((phrase (noun plural) ("cat"))))))

(ert-deftest eprolog-usage-dcg-semantic-actions ()
  "Test DCG semantic actions."
  (eprolog-test--restore-builtins)
  
  ;; Test semantic actions
  (eprolog-define-grammar! (s _num) (np _num) (vp _num))
  (eprolog-define-grammar! (np _num) (det _num) (noun _num))
  (eprolog-define-grammar! (vp _num) (verb _num) (np _))
  (eprolog-define-grammar! (det singular) "a")
  (eprolog-define-grammar (det plural) "some")
  (eprolog-define-grammar! (noun singular) "cat")
  (eprolog-define-grammar (noun plural) "cats")
  (eprolog-define-grammar! (verb singular) "chases")
  (eprolog-define-grammar (verb plural) "chase")
  
  (should (eprolog-test--has-solution-p '((phrase (s _) ("a" "cat" "chases" "some" "cats")))))
  (should-not (eprolog-test--has-solution-p '((phrase (s _) ("a" "cat" "chase" "some" "cats"))))))

(ert-deftest eprolog-usage-dcg-cut-operations ()
  "Test DCG cut operations."
  (eprolog-test--restore-builtins)
  
  ;; Test cut operations
  (eprolog-define-grammar! statement declarative !)
  (eprolog-define-grammar statement question)
  (eprolog-define-grammar! declarative s ".")
  (eprolog-define-grammar! question s "?")
  (eprolog-define-grammar! s "test")
  
  (should (eprolog-test--has-solution-p '((phrase statement ("test" "."))))))

(ert-deftest eprolog-usage-dcg-generation ()
  "Test DCG sentence generation."
  (eprolog-test--restore-builtins)
  
  ;; Define simple grammar
  (eprolog-define-grammar! s np vp)
  (eprolog-define-grammar! np det noun)
  (eprolog-define-grammar! vp verb)
  (eprolog-define-grammar! det "the")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar! verb "runs")
  
  ;; Test sentence generation
  (let ((solutions (eprolog-test--collect-solutions '((phrase s _sentence)))))
    (should (> (length solutions) 0))
    (should (equal (cdr (assoc '_sentence (car solutions))) '("the" "cat" "runs")))))

(ert-deftest eprolog-usage-dcg-length-constrained-generation ()
  "Test DCG generation with length constraints."
  (eprolog-test--restore-builtins)
  
  ;; Test length-constrained generation
  (eprolog-define-predicate (length () 0))
  (eprolog-define-predicate (length (_h . _t) _n)
    (length _t _n1)
    (is _n (+ _n1 1)))
  
  (eprolog-define-grammar! s2 det noun verb)
  (eprolog-define-grammar! det "a")
  (eprolog-define-grammar! noun "cat")
  (eprolog-define-grammar! verb "runs")
  
  (let ((solutions (eprolog-test--collect-solutions '((phrase s2 _sentence) (length _sentence 3)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_sentence (car solutions))) '("a" "cat" "runs")))))

(ert-deftest eprolog-usage-dcg-arithmetic-expressions ()
  "Test arithmetic expression parsing with DCG."
  (eprolog-test--restore-builtins)
  
  ;; Define arithmetic grammar
  (eprolog-define-grammar! expr term)
  (eprolog-define-grammar expr term "+" expr)
  (eprolog-define-grammar expr term "-" expr)
  (eprolog-define-grammar! term factor)
  (eprolog-define-grammar term factor "*" term)
  (eprolog-define-grammar term factor "/" term)
  (eprolog-define-grammar! factor digit)
  (eprolog-define-grammar factor "(" expr ")")
  (eprolog-define-grammar! digit "1")
  (eprolog-define-grammar digit "2")
  (eprolog-define-grammar digit "3")
  (eprolog-define-grammar digit "4")
  
  ;; Test arithmetic expression parsing
  (should (eprolog-test--has-solution-p '((phrase expr ("2")))))
  (should (eprolog-test--has-solution-p '((phrase expr ("2" "+" "3" "*" "4")))))
  (should (eprolog-test--has-solution-p '((phrase expr ("(" "2" "+" "3" ")" "*" "4"))))))

(ert-deftest eprolog-usage-dcg-nested-structures ()
  "Test nested structure parsing with DCG."
  (eprolog-test--restore-builtins)
  
  ;; Define nested parentheses grammar
  (eprolog-define-grammar! parens nil)
  (eprolog-define-grammar parens "(" parens ")" parens)
  
  ;; Test balanced parentheses
  (should (eprolog-test--has-solution-p '((phrase parens ()))))
  (should (eprolog-test--has-solution-p '((phrase parens ("(" ")")))))
  (should (eprolog-test--has-solution-p '((phrase parens ("(" "(" ")" "(" ")" ")")))))
  (should-not (eprolog-test--has-solution-p '((phrase parens ("(" "(" ")"))))))

(ert-deftest eprolog-usage-dcg-csv-parsing ()
  "Test CSV-style parsing with DCG."
  (eprolog-test--restore-builtins)
  
  ;; Test CSV-style parsing
  (eprolog-define-grammar! csv-list item)
  (eprolog-define-grammar csv-list item "," csv-list)
  (eprolog-define-grammar! item "apple")
  (eprolog-define-grammar item "banana")
  (eprolog-define-grammar item "cherry")
  
  (should (eprolog-test--has-solution-p '((phrase csv-list ("apple")))))
  (should (eprolog-test--has-solution-p '((phrase csv-list ("apple" "," "banana" "," "cherry"))))))

(ert-deftest eprolog-usage-family-tree-comprehensive ()
  "Test comprehensive family tree with Sazae-san characters."
  (eprolog-test--restore-builtins)
  
  ;; Define parent relationships
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent ikura taiko))
  
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
  
  ;; Test basic relationships
  (should (eprolog-test--has-solution-p '((parent katsuo fune))))
  (should (eprolog-test--has-solution-p '((married fune namihei))))
  (should (eprolog-test--has-solution-p '((male katsuo))))
  (should (eprolog-test--has-solution-p '((female wakame)))))

(ert-deftest eprolog-usage-family-tree-derived-relationships ()
  "Test derived family relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup basic relationships
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  
  (eprolog-define-prolog-predicate! (male katsuo))
  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female wakame))
  
  ;; Define derived relationships
  (eprolog-define-prolog-predicate! (child _x _y) (parent _y _x))
  (eprolog-define-prolog-predicate! (grandparent _x _z) (parent _x _y) (parent _y _z))
  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))
  (eprolog-define-prolog-predicate! (mother _x _y) (parent _x _y) (female _y))
  (eprolog-define-prolog-predicate! (father _x _y) (parent _x _y) (male _y))
  (eprolog-define-prolog-predicate! (sister _x _y) (sibling _x _y) (female _y))
  (eprolog-define-prolog-predicate! (brother _x _y) (sibling _x _y) (male _y))
  (eprolog-define-prolog-predicate! (ancestor _x _y) (parent _x _y))
  (eprolog-define-prolog-predicate (ancestor _x _y) (parent _x _z) (ancestor _z _y))
  
  ;; Test derived relationships
  (should (eprolog-test--has-solution-p '((child fune katsuo))))
  (should (eprolog-test--has-solution-p '((grandparent tarao fune))))
  (should (eprolog-test--has-solution-p '((sibling katsuo wakame))))
  (should (eprolog-test--has-solution-p '((mother katsuo fune))))
  (should (eprolog-test--has-solution-p '((sister katsuo wakame))))
  (should (eprolog-test--has-solution-p '((brother wakame katsuo))))
  (should (eprolog-test--has-solution-p '((ancestor tarao fune)))))

(ert-deftest eprolog-usage-family-tree-uncle-aunt-cousin ()
  "Test uncle, aunt, and cousin relationships."
  (eprolog-test--restore-builtins)
  
  ;; Setup family tree with extended relationships
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent child1 katsuo))
  (eprolog-define-prolog-predicate (parent child2 wakame))
  
  (eprolog-define-prolog-predicate! (male katsuo))
  (eprolog-define-prolog-predicate! (female fune))
  (eprolog-define-prolog-predicate (female sazae))
  (eprolog-define-prolog-predicate (female wakame))
  
  ;; Define extended relationships
  (eprolog-define-prolog-predicate! (sibling _x _y) (parent _x _z) (parent _y _z) (not (= _x _y)))
  (eprolog-define-prolog-predicate! (uncle _x _y) (parent _y _z) (sibling _x _z) (male _x))
  (eprolog-define-prolog-predicate! (aunt _x _y) (parent _y _z) (sibling _x _z) (female _x))
  (eprolog-define-prolog-predicate! (cousin _x _y) (parent _x _a) (parent _y _b) (sibling _a _b))
  
  ;; Test uncle/aunt relationships
  (should (eprolog-test--has-solution-p '((uncle katsuo tarao))))
  (should (eprolog-test--has-solution-p '((aunt wakame tarao))))
  
  ;; Test cousin relationships  
  (should (eprolog-test--has-solution-p '((cousin tarao child1))))
  (should (eprolog-test--has-solution-p '((cousin child1 child2)))))

(ert-deftest eprolog-usage-family-tree-complex-queries ()
  "Test complex family tree queries with multiple solutions."
  (eprolog-test--restore-builtins)
  
  ;; Setup complete family tree
  (eprolog-define-prolog-predicate! (parent katsuo fune))
  (eprolog-define-prolog-predicate (parent wakame fune))
  (eprolog-define-prolog-predicate (parent sazae fune))
  (eprolog-define-prolog-predicate (parent tarao sazae))
  (eprolog-define-prolog-predicate (parent ikura taiko))
  
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

(ert-deftest eprolog-usage-complex-backtracking-with-cut ()
  "Test complex backtracking scenarios with cut."
  (eprolog-test--restore-builtins)
  
  ;; Define choice predicates
  (eprolog-define-prolog-predicate! (color red))
  (eprolog-define-prolog-predicate (color green))
  (eprolog-define-prolog-predicate (color blue))
  
  ;; Define predicate that uses cut
  (eprolog-define-prolog-predicate! (first-color _x)
    (color _x) !)
  
  ;; Test without cut - should get all solutions
  (let ((solutions (eprolog-test--collect-solutions '((color _x)))))
    (should (= (length solutions) 3)))
  
  ;; Test with cut - should get only first solution
  (let ((solutions (eprolog-test--collect-solutions '((first-color _x)))))
    (should (= (length solutions) 1))
    (should (equal (cdr (assoc '_x (car solutions))) 'red))))

(ert-deftest eprolog-usage-repeat-with-complex-conditions ()
  "Test repeat predicate with complex termination conditions."
  (eprolog-test--restore-builtins)
  
  ;; Test repeat with counter and cut
  (let ((counter 0))
    (eprolog-define-predicate (test-repeat-complex)
      (repeat)
      (lisp! (setq counter (1+ counter)))
      (lispp (>= counter 5))
      !)
    
    (should (eprolog-test--has-solution-p '((test-repeat-complex))))
    (should (= counter 5))))

(ert-deftest eprolog-usage-factorial ()
  "Test recursive factorial implementation."
  (eprolog-test--restore-builtins)
  
  ;; Define factorial predicate
  (eprolog-define-predicate! (factorial 0 1))
  (eprolog-define-predicate (factorial _n _f)
    (lispp (> _n 0))
    (is _n1 (- _n 1))
    (factorial _n1 _f1)
    (is _f (* _n _f1)))
  
  ;; Test factorial calculations
  (let ((solutions (eprolog-test--collect-solutions '((factorial 0 _f)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_f (car solutions))) 1)))
  
  (let ((solutions (eprolog-test--collect-solutions '((factorial 3 _f)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_f (car solutions))) 6)))
  
  (let ((solutions (eprolog-test--collect-solutions '((factorial 5 _f)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_f (car solutions))) 120))))

(ert-deftest eprolog-usage-fibonacci ()
  "Test Fibonacci sequence implementation."
  (eprolog-test--restore-builtins)
  
  ;; Define Fibonacci predicate
  (eprolog-define-predicate! (fib 0 0))
  (eprolog-define-predicate (fib 1 1))
  (eprolog-define-predicate (fib _n _f)
    (lispp (> _n 1))
    (is _n1 (- _n 1))
    (is _n2 (- _n 2))
    (fib _n1 _f1)
    (fib _n2 _f2)
    (is _f (+ _f1 _f2)))
 
  ;; Test Fibonacci calculations
  (let ((solutions (eprolog-test--collect-solutions '((fib 0 _f)))))
    (should (= (cdr (assoc '_f (car solutions))) 0)))
  
  (let ((solutions (eprolog-test--collect-solutions '((fib 1 _f)))))
    (should (= (cdr (assoc '_f (car solutions))) 1)))
  
  (let ((solutions (eprolog-test--collect-solutions '((fib 3 _f)))))
    (should (= (cdr (assoc '_f (car solutions))) 2)))
  
  (let ((solutions (eprolog-test--collect-solutions '((fib 4 _f)))))
    (should (= (cdr (assoc '_f (car solutions))) 3))))

(ert-deftest eprolog-usage-gcd-algorithm ()
  "Test Greatest Common Divisor algorithm."
  (eprolog-test--restore-builtins)
  
  ;; Define GCD predicate
  (eprolog-define-predicate! (gcd _a 0 _a))
  (eprolog-define-predicate (gcd _a _b _g)
    (lispp (> _b 0))
    (is _r (mod _a _b))
    (gcd _b _r _g))
  
  ;; Test GCD calculations
  (let ((solutions (eprolog-test--collect-solutions '((gcd 48 18 _g)))))
    (should (= (cdr (assoc '_g (car solutions))) 6)))
  
  (let ((solutions (eprolog-test--collect-solutions '((gcd 15 25 _g)))))
    (should (= (cdr (assoc '_g (car solutions))) 5))))

(ert-deftest eprolog-usage-performance-tests ()
  "Test performance with larger databases and deep recursion."
  (eprolog-test--restore-builtins)
  
  ;; Test large database performance
  (dotimes (i 100)
    (eval `(eprolog-define-predicate (test-num ,i))))
  
  (let ((solutions (eprolog-test--collect-solutions '((test-num _x)))))
    (should (= (length solutions) 100)))
  
  ;; Test deep recursion
  (eprolog-define-predicate! (count-down 0))
  (eprolog-define-predicate (count-down _n)
    (lispp (> _n 0))
    (is _n1 (- _n 1))
    (count-down _n1))
  
  (should (eprolog-test--has-solution-p '((count-down 10)))))

(ert-deftest eprolog-usage-large-database ()
  "Test performance with larger clause database."
  (eprolog-test--restore-builtins)
  (dotimes (i 100)
    (eval `(eprolog-define-predicate (test-num ,i))))
  
  (let ((solutions (eprolog-test--collect-solutions '((test-num _x)))))
    (should (= (length solutions) 100))))

(ert-deftest eprolog-usage-stress-testing ()
  "Test system behavior under stress conditions."
  (eprolog-test--restore-builtins)
  
  ;; Test many predicate clauses
  (dotimes (i 50)
    (eval `(eprolog-define-predicate (many-choices ,i))))
  
  ;; Test that all solutions are found
  (let ((solutions (eprolog-test--collect-solutions '((many-choices _x)))))
    (should (= (length solutions) 50)))
  
  ;; Test complex recursive predicate
  (eprolog-define-predicate! (deep-recursion 0 done))
  (eprolog-define-predicate (deep-recursion _n _result)
    (lispp (> _n 0))
    (is _n1 (- _n 1))
    (deep-recursion _n1 _result))
  
  ;; Test with reduced recursion depth to avoid eval depth limit
  (should (eprolog-test--has-solution-p '((deep-recursion 10 done)))))

(defun eprolog-usage-run-all-tests ()
  "Run all ε-prolog usage tests and display summary."
  (interactive)
  (let ((start-time (current-time)))
    (ert-run-tests-batch-and-exit "eprolog-usage-")
    (message "ε-prolog usage tests completed in %.2f seconds" 
             (float-time (time-subtract (current-time) start-time)))))

(provide 'eprolog-usage-tests)
