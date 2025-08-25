
# Introduction

Welcome to the comprehensive guide and test suite for ε-prolog, a sophisticated implementation of Prolog within the Emacs Lisp ecosystem. This document bridges theoretical understanding and practical application by combining educational examples with executable tests using ERT (Emacs Lisp Regression Testing).

What makes this document unique is its dual nature: every example is both a learning tool and a working test case. As you read through the explanations and examples, you're also examining a complete test suite that validates ε-prolog's functionality. This approach ensures that all examples are accurate, up-to-date, and actually work as described.

The document draws from practical examples in README.org and the implementation details in eprolog.el, presenting them in a structured, progressive manner that builds your understanding from basic concepts to advanced applications.

## Quick Start

ε-prolog provides convenient aliases for commonly used functions:

-   `eprolog-define-predicate` for `eprolog-define-prolog-predicate`
-   `eprolog-define-predicate!` for `eprolog-define-prolog-predicate!`

### Basic Examples

Here are the fundamental examples that demonstrate how to define facts, rules, and query the knowledge base:

```
;; Define facts (using convenient alias)
(eprolog-define-predicate (parent tom bob))
(eprolog-define-predicate (parent tom liz))
(eprolog-define-predicate (parent bob ann))
(eprolog-define-predicate (parent bob pat))
(eprolog-define-predicate (parent pat jim))

;; Define rules
(eprolog-define-predicate (grandparent _x _z)
  (parent _x _y)
  (parent _y _z))

;; Query the database
(eprolog-query (grandparent tom _x))
;; Returns: _x = ann, _x = pat
```

### Built-in Predicates Examples

```
;; List operations
(eprolog-query (member _x (a b c)))
;; Returns: _x = a, _x = b, _x = c

;; Append lists
(eprolog-query (append (1 2) (3 4) _result))
;; Returns: _result = (1 2 3 4)

;; Type checking
(eprolog-query (atom foo))     ;; Returns: t
(eprolog-query (number 42))    ;; Returns: t
(eprolog-query (var _x))       ;; Returns: t
```

### Lisp Integration Examples

```
;; Lisp expression evaluation
(eprolog-query (lisp _result (+ 2 3)))
;; Returns: _result = 5

;; Conditional evaluation
(eprolog-query (lispp (> 5 3)))
;; Returns: t

;; Side effects
(eprolog-query (lisp! (message "Hello from Prolog!")))
```

## Running Tests

All examples in this documentation can be executed as tests using ERT (Emacs Lisp Regression Testing):

```
;; Load the main file and documentation
(load "eprolog.el")
(org-babel-load-file "docs/index.org")

;; Run all tests
(ert-run-tests-batch-and-exit "eprolog-usage-")
```

## Documentation Structure

The guide is organized into thematic parts:

### Part I: Core Concepts

-   [Core Prolog Functionality](core-prolog.md) - Facts, rules, unification, and basic Prolog operations
-   [Built-in Predicates](builtin-predicates.md) - Type checking, list operations, and higher-order predicates
-   [Control Flow](control-flow.md) - Cut, backtracking, logical operators, and meta-predicates
-   [Arithmetic and Mathematics](arithmetic.md) - Mathematical operations and comparisons

### Part II: Advanced Features

-   [Lisp Integration](lisp-integration.md) - Seamless Prolog-Lisp interoperability
-   [Dynamic Parameters](dynamic-parameters.md) - Stateful data with backtracking-aware semantics
-   [Definite Clause Grammars](dcg.md) - Grammar definition and parsing capabilities

### Part III: Examples

-   [Complex Examples](examples.md) - Advanced applications, family trees, and recursive algorithms

### Part IV: Reference

-   [API Reference](api-reference.md) - Complete function and predicate documentation

# Setup

Before exploring the specific modules, let's establish the testing environment that all examples will use:

```emacs-lisp
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
```

