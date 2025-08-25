
# Dynamic Parameters

Dynamic parameters provide a sophisticated stateful mechanism for maintaining and sharing data across different parts of a Prolog computation. They bridge the gap between Prolog's typically stateless nature and practical needs for accumulating results, counters, and complex data sharing.

## Core Predicates

### `store/2`: Store a value with backtracking support

`store(Key, Value)` stores `Value` under `Key` in the dynamic parameter store. The value persists during forward execution and is automatically restored to its previous state when backtracking occurs above the store operation.

### `fetch/2`: Retrieve stored values

`fetch(Key, Variable)` retrieves the value associated with `Key` and unifies it with `Variable`. Fails if the key doesn't exist in the current parameter context.

## Key Features

-   ****Backtracking-Aware****: Parameter values are automatically restored during backtracking
-   ****Scoped Storage****: Each store operation creates a new parameter scope
-   ****Lisp Integration****: Values can be any Lisp data structure
-   ****Query-Local****: Parameters exist only within the current query execution
-   ****Deterministic Access****: fetch/2 is deterministic - succeeds once or fails

## Common Use Cases

1.  ****Counters and Accumulators****: Maintaining running totals across recursive predicates
2.  ****State Machines****: Tracking current state in complex logic
3.  ****Caching****: Storing computed values for reuse within a query
4.  ****Inter-Predicate Communication****: Sharing data between unrelated predicates
5.  ****Configuration****: Passing settings through deep call stacks

## Usage Examples

### Basic Storage and Retrieval

```
;; Store a simple value
(eprolog-query '((store user-id 12345)
                 (fetch user-id _id)
                 (lisp _result (format "User: %d" _id))))
;; Result: _result = "User: 12345"
```

### Counter Pattern

```
;; Initialize and increment a counter
(eprolog-define-predicate (increment-counter _new-value)
  (fetch counter _current)
  (is _new-value (+ _current 1))
  (store counter _new-value))

(eprolog-query '((store counter 0)
                 (increment-counter _val1)  ; Sets counter to 1
                 (increment-counter _val2)  ; Sets counter to 2
                 (fetch counter _final)))   ; _final = 2
```

### Backtracking Behavior

```
;; Demonstrates automatic restoration during backtracking
(eprolog-query '((store level start)
                 (or (and (store level branch-a)
                          (fetch level _val))     ; _val = branch-a
                     (and (store level branch-b)  
                          (fetch level _val))     ; _val = branch-b
                     (fetch level _val))))       ; _val = start (restored)
;; Produces 3 solutions: branch-a, branch-b, start
```

### Complex Data Structures

```
;; Store and manipulate complex Lisp data
(eprolog-query '((store config (list :debug t :max-depth 10))
                 (fetch config _cfg)
                 (lisp _debug-mode (plist-get _cfg :debug))
                 (lisp _max (plist-get _cfg :max-depth))))
```

## Best Practices

### Use Descriptive Keys

```
;; Good: Descriptive key names
(store user-preferences (list :theme dark :font-size 12))
(store current-search-depth 5)

;; Avoid: Generic or unclear keys  
(store x 42)
(store temp-var some-value)
```

### Initialize Before Use

```
;; Always initialize parameters at query start
(eprolog-define-predicate (process-with-counter _items _result)
  (store item-count 0)           ; Initialize counter
  (process-list _items _result)) ; Then process
```

### Handle Missing Keys Gracefully

```
;; Use default values for optional parameters
(eprolog-define-predicate (get-config-value _key _value)
  (or (fetch _key _value)           ; Try to fetch
      (= _value default-value)))    ; Use default if not found
```

## Backtracking Semantics

The store/fetch mechanism implements ****scoped parameter restoration****: when backtracking occurs, parameter values are restored to their state at the time of the choice point, not completely removed. This creates a stack-like behavior where each choice point preserves its parameter context.

```
;; Example demonstrating scoped restoration
(eprolog-query 
  '((store level 0)                    ; Initial: level=0
    (or (and (store level 1)           ; Branch 1: level=1
             (or (store level 2)       ;   Sub-branch: level=2
                 (store level 3))      ;   Sub-branch: level=3  
             (fetch level _inner))     ; _inner gets 2 or 3
        (fetch level _outer))))        ; _outer gets 0 (restored)
;; Solutions: _inner=2, _inner=3, _outer=0
```

## Integration with Lisp

Dynamic parameters seamlessly integrate with Lisp evaluation, allowing storage and retrieval of any Lisp data structure:

```
;; Store complex Lisp structures
(eprolog-query '((lisp _hash (make-hash-table))
                 (store shared-data _hash)
                 (fetch shared-data _retrieved)
                 (lisp! (puthash 'key 'value _retrieved))))

;; Store and retrieve functions
(eprolog-query '((store formatter (lambda (x) (format "Value: %s" x)))
                 (fetch formatter _fn)
                 (lisp _result (funcall _fn 42))))
```

## Core Functionality Tests

The following tests demonstrate and validate the fundamental behavior of dynamic parameters:

```emacs-lisp
(ert-deftest eprolog-feature-lisp-dynamic-parameters ()
  "Test core dynamic parameter functionality including storage, retrieval, and backtracking."
  (eprolog-test--restore-builtins)

  ;; Test store and fetch
  (let ((solutions (eprolog-test--collect-solutions
           '((store test-key 42)
             (fetch test-key _value)
             (= _value 42)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_value (car solutions))) 42)))

  ;; Test parameter persistence across goals
  (let ((solutions (eprolog-test--collect-solutions
           '((store counter 0)
             (fetch counter _old)
             (is _new (+ _old 1))
             (store counter _new)
             (fetch counter 1)))))
    (should (= (length solutions) 1)))

  ;; Test backtracking restores previous values
  (let ((solutions (eprolog-test--collect-solutions
           '((store a 0)
             (or (store a 1) true)
             (fetch a _v)))))
    (should (= (length solutions) 2))
    (should (equal (mapcar (lambda (sol) (cdr (assoc '_v sol))) solutions)
                  '(1 0)))))
```

# Advanced Store/Fetch Backtracking Tests

The store/fetch mechanism must handle complex backtracking scenarios correctly, ensuring that parameter values are properly restored when execution backtracks above store operations.

```emacs-lisp
(ert-deftest eprolog-feature-store-fetch-advanced-backtracking ()
  "Test comprehensive backtracking scenarios for store/fetch predicates."
  (eprolog-test--restore-builtins)

  ;; === TEST 1: Nested backtracking with multiple store operations ===
  ;; This tests that stores at different backtrack points are restored correctly
  (let ((solutions (eprolog-test--collect-solutions
           '((store level 0)
             (or (and (store level 1)
                      (or (store level 2) (store level 3))
                      (fetch level _v))
                 (fetch level _v))))))
    (should (= (length solutions) 3))
    ;; Should get: level=2, level=3, level=0
    (let ((values (mapcar (lambda (sol) (cdr (assoc '_v sol))) solutions)))
      (should (equal (sort values #'<) '(0 2 3)))))

  ;; === TEST 2: Complex choice points with store operations ===
  ;; Test store operations inside multiple choice alternatives
  (let ((solutions (eprolog-test--collect-solutions
           '((store base 100)
             (member _x (1 2 3))
             (lisp _offset (* _x 10))
             (store temp _offset)
             (fetch base _base)
             (fetch temp _temp)
             (is _result (+ _base _temp))))))
    (should (= (length solutions) 3))
    (let ((results (mapcar (lambda (sol) (cdr (assoc '_result sol))) solutions)))
      (should (equal (sort results #'<) '(110 120 130)))))

  ;; === TEST 3: Deep backtracking chains ===
  ;; Multiple levels of nested backtracking with parameter restoration
  (let ((solutions (eprolog-test--collect-solutions
           '((store depth 0)
             (or (and (store depth 1)
                      (or (and (store depth 2)
                               (or (store depth 3) (store depth 4)))
                          (store depth 5)))
                 (store depth 6))
             (fetch depth _final)))))
    (should (= (length solutions) 4))
    (let ((depths (mapcar (lambda (sol) (cdr (assoc '_final sol))) solutions)))
      (should (equal (sort depths #'<) '(3 4 5 6)))))

  ;; === TEST 4: Store same key multiple times in different branches ===
  ;; Ensure proper restoration when same key is stored multiple times
  (let ((solutions (eprolog-test--collect-solutions
           '((store counter 0)
             (or (and (store counter 10)
                      (store counter 11)
                      (fetch counter _value))
                 (and (store counter 20)
                      (store counter 21) 
                      (fetch counter _value))
                 (fetch counter _value))))))
    (should (= (length solutions) 3))
    (let ((values (mapcar (lambda (sol) (cdr (assoc '_value sol))) solutions)))
      (should (equal (sort values #'<) '(0 11 21)))))

  ;; === TEST 5: Backtracking with failed fetch operations ===
  ;; Test behavior when fetch operations fail during backtracking
  ;; The successful branch should produce one solution, failed fetch should be unbound
  (let ((solutions (eprolog-test--collect-solutions
           '((or (and (store temp-key 42)
                      (fetch temp-key _success))
                 (fetch nonexistent-key _fail))))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_success (car solutions))) 42))
    ;; The _fail variable should be unbound (equal to itself) since that branch failed
    (should (eq (cdr (assoc '_fail (car solutions))) '_fail)))

  ;; === TEST 6: Complex data structures in backtracking ===
  ;; Test backtracking with complex stored values
  (let ((solutions (eprolog-test--collect-solutions
           '((store data (initial))
             (member _item (a b c))
             (lisp _new_data (list 'updated '_item))
             (store data _new_data)
             (fetch data _result)))))
    (should (= (length solutions) 3))
    (let ((results (mapcar (lambda (sol) (cdr (assoc '_result sol))) solutions)))
      (should (equal results '((updated a) (updated b) (updated c))))))

  ;; === TEST 7: Store operations followed by cuts ===
  ;; Test that cuts don't interfere with parameter restoration
  (eprolog-define-predicate (cut-test _result)
    (store cut-test-key 1)
    (member _x (a b c))
    (= _x b)
    !
    (fetch cut-test-key _result))

  (let ((solutions (eprolog-test--collect-solutions '((cut-test _val)))))
    (should (= (length solutions) 1))
    (should (= (cdr (assoc '_val (car solutions))) 1)))

  ;; === TEST 8: Multiple independent parameter stores ===
  ;; Test multiple parameters being stored and restored independently
  (let ((solutions (eprolog-test--collect-solutions
           '((store x 10)
             (store y 20)
             (or (and (store x 11) (store y 21) (fetch x _x1) (fetch y _y1))
                 (and (store x 12) (fetch x _x2) (fetch y _y2))
                 (and (fetch x _x3) (fetch y _y3)))))))
    (should (= (length solutions) 3))
    ;; Verify each solution has the correct parameter values
    (let* ((sol1 (nth 0 solutions))
           (sol2 (nth 1 solutions))
           (sol3 (nth 2 solutions)))
      (should (and (= (cdr (assoc '_x1 sol1)) 11) (= (cdr (assoc '_y1 sol1)) 21)))
      (should (and (= (cdr (assoc '_x2 sol2)) 12) (= (cdr (assoc '_y2 sol2)) 20)))
      (should (and (= (cdr (assoc '_x3 sol3)) 10) (= (cdr (assoc '_y3 sol3)) 20))))))
```

# Dynamic Parameters Negative Tests

Dynamic parameter predicates should fail with invalid keys or expressions:

```emacs-lisp
(ert-deftest eprolog-feature-lisp-fetch-negative-tests ()
  "Test negative cases for fetch predicate."
  (eprolog-test--restore-builtins)

  ;; Getting non-existent keys should fail
  (should-not (eprolog-test--has-solution-p '((fetch nonexistent-key _value))))
  (should-not (eprolog-test--has-solution-p '((fetch missing-key _x))))

  ;; Unification failures with retrieved values
  (let ((solutions (eprolog-test--collect-solutions '((store test-key 42)))))
    (should (= (length solutions) 1)))
  (should-not (eprolog-test--has-solution-p '((fetch test-key "forty-two"))))
  (should-not (eprolog-test--has-solution-p '((fetch test-key (a b c))))))
```

