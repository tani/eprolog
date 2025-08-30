

# ε-prolog

[![img](https://melpa.org/packages/eprolog-badge.svg)](https://melpa.org/#/eprolog)
[![img](https://deepwiki.com/badge.svg)](https://deepwiki.com/tani/eprolog)

ε-prolog (`eprolog`) is a complete Prolog engine implementation written in pure Emacs Lisp. It provides a fully functional Prolog system integrated into the Emacs environment, offering traditional Prolog programming capabilities with seamless Lisp interoperability.

# Features

## Core Prolog Engine

-   Complete unification algorithm with occurs check
-   Backtracking and choice points with proper cut (!) semantics
-   Clause database management for facts and rules
-   Interactive query execution with solution enumeration

## Built-in Predicates

-   Unification: `=`, `==`
-   Type checking: `atom/1`, `atomic/1`, `var/1`, `ground/1`, `number/1`, `string/1`
-   Control: `fail/0`, `!/0`, `true/0`, `false/0`, `not/1` (negation as failure)
-   Logical operators: `and/0+`, `or/0+`, `if/2-3`
-   List operations: `member/2`, `append/2-3`, `maplist/2-5`
-   Meta-call: `call/1+`
-   Arithmetic: `is/2` which is an alias of `lisp/2+`
-   Lisp integration: `lisp/2+`, `lisp!/1+`, `lispp/1+`
-   Dynamic parameters: `store/2`, `fetch/2` for stateful computation

## Advanced Features

-   Definite Clause Grammar (DCG) support
-   Spy/debugging functionality for tracing execution
-   Dynamic parameters (`store/2`, `fetch/2`) with backtracking-aware state management
-   Anonymous variable handling
-   Variable renaming for clause application

# Requirements

ε-prolog requires Emacs 27.2 or later. While the package is functional from version 27.2, **Emacs 29+ is recommended** for optimal performance due to improved stack frame handling that enables deeper recursion.

# Installation

Place `eprolog.el` in your Emacs load path and add to your configuration:

```
(require 'eprolog)
```

# Quick Start

For detailed examples, tutorials, and comprehensive documentation, see the docs/ directory.

Basic usage involves defining facts and rules, then querying the knowledge base:

```
;; Define facts
(eprolog-define-predicate (parent tom bob))
(eprolog-define-predicate (parent bob ann))

;; Define rules  
(eprolog-define-predicate (grandparent _x _z)
  (parent _x _y)
  (parent _y _z))

;; Query the database
(eprolog-query (grandparent tom _x))
;; Returns: _x = ann

;; Dynamic parameters for stateful computation
(eprolog-query 
 (store counter 0)
 (fetch counter _old)
 (is _new (+ _old 1))
 (store counter _new)
 (fetch counter _result))
;; Returns: _result = 1
```

ε-prolog provides convenient aliases:

-   `eprolog-define-predicate` for `eprolog-define-prolog-predicate`
-   `eprolog-define-predicate!` for `eprolog-define-prolog-predicate!`

For complete API documentation, advanced examples, and comprehensive tutorials, see the docs/ directory.

# Testing

ε-prolog includes a comprehensive test suite in the docs/ directory with tests covering all major functionality.

Run tests with:

```
make test
```

See the docs/ directory for detailed testing instructions and interactive test examples.

# Debugging

Enable spy mode to trace predicate execution:

```
(setq eprolog-spy-predicates '(grandparent parent))
(setq eprolog-spy-state 'prompt) ; or 'always or 'disabled
```

See the docs/ directory for comprehensive debugging examples and techniques.

# Implementation Details

ε-prolog implements a complete Prolog engine with:

-   Unification: Standard unification algorithm with optional occurs check
-   Proof Search: Depth-first search with backtracking via continuations
-   Cut Implementation: Proper cut semantics using exception handling
-   Variable Scoping: Automatic variable renaming for clause application
-   Success/Failure Types: Explicit representation of computation results

The engine uses continuation-passing style for backtracking, making the implementation both elegant and efficient within Emacs Lisp's constraints.

# License

ε-prolog is released under the GNU General Public License v3.0. See LICENSE.org for details.

# Historical Note

This implementation has an interesting lineage.
It originally derives from Peter Norvig's Prolog implementation in [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp) ([MIT licensed](https://github.com/norvig/paip-lisp/blob/9cea73837e439d331fe78d7b585e994c7113aac2/LICENSE)).
The code was first reimplemented in Scheme as [Athena](https://github.com/tani/athena), then ported back to Common Lisp,
and finally adapted for Emacs Lisp as ε-prolog.
Each iteration refined the implementation while maintaining the core algorithmic elegance of the original.

# Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

# Author

Masaya Taniguchi

# Acknowledgments

This implementation draws inspiration from classical Prolog systems and modern functional programming techniques, adapted specifically for the Emacs Lisp environment.

