# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ε-prolog is a complete Prolog engine implementation written in pure Emacs Lisp. It provides a fully functional Prolog system integrated into the Emacs environment with traditional Prolog programming capabilities and seamless Lisp interoperability.

## Development Commands

### Testing
- `make test` - Run the complete test suite with modular documentation
- `make` or `make all` - Default target that runs tests

### Code Quality
- `make check` - Run byte compilation and documentation validation
- `make format` - Format source code with proper indentation

### Build Management
- `make clean` - Remove generated documentation files (docs/*.el)

### Documentation Generation
The project uses a unique literate programming approach where documentation in `docs/*.org` files contains both explanations and executable test cases. The build system automatically generates corresponding `.el` files from these org files using org-babel.

## Architecture

### Core Implementation (eprolog.el)
The main engine is implemented as a single file with these key components:

**Data Types:**
- `eprolog--success` and `eprolog--failure` structs represent computation results
- Continuation-passing style for backtracking implementation
- Cut implementation using exception handling (`eprolog-cut-exception`)

**Core Engine:**
- **Unification:** Standard algorithm with optional occurs check (`eprolog-occurs-check`)
- **Proof Search:** Depth-first search with backtracking via continuations  
- **Variable Management:** Variables begin with `_`, anonymous variables handled specially
- **Clause Database:** Global `eprolog-clause-database` stores facts and rules

**Key Functions:**
- `eprolog--unify` - Core unification with occurs check
- `eprolog--prove-goal` - Main goal proving engine
- `eprolog--substitute-bindings` - Apply variable bindings to expressions
- `eprolog--rename-vars` - Variable scoping for clause application

### Public API
- `eprolog-define-prolog-predicate` - Define facts and rules (aliased as `eprolog-define-predicate`)
- `eprolog-define-prolog-predicate!` - Replace existing predicates (aliased as `eprolog-define-predicate!`)
- `eprolog-query` - Execute queries and get solutions
- `eprolog-define-grammar` / `eprolog-define-grammar!` - DCG (Definite Clause Grammar) support

### Built-in Predicates
Extensive built-in predicate library including:
- **Unification:** `=`, `==`
- **Type checking:** `atom/1`, `var/1`, `ground/1`, `number/1`, etc.
- **Control:** `fail/0`, `!/0`, `true/0`, `not/1`
- **Logic:** `and/0+`, `or/0+`, `if/2-3`
- **Lists:** `member/2`, `append/2-3`, `maplist/2-5`
- **Lisp integration:** `lisp/2+`, `lisp!/1+`, `lispp/1+`
- **Arithmetic:** `is/2` (alias for `lisp/2+`)

### Documentation System
The `docs/` directory contains a comprehensive test suite implemented as literate programming:
- Each `.org` file combines documentation with executable ERT tests
- `docs/index.org` - Main guide and basic examples
- Subject-specific modules: `core-prolog.org`, `dcg.org`, `examples.org`, etc.
- Tests are generated automatically and run via the Makefile

### Configuration
Key variables for engine behavior:
- `eprolog-occurs-check` - Enable/disable occurs check in unification
- `eprolog-spy-predicates` - List of predicates to debug
- `eprolog-spy-state` - Debug mode (`'prompt`, `'always`, or `'disabled`)

## Development Practices

### Code Style
- Uses `cl-lib` extensively for Common Lisp compatibility
- Lexical binding enabled throughout
- Internal functions prefixed with `eprolog--`
- Comprehensive docstrings following Emacs conventions

### Testing Strategy
The project employs a unique approach where all documentation examples are also executable tests using ERT (Emacs Lisp Regression Testing). This ensures documentation accuracy and provides comprehensive test coverage.

### Dependencies
- Requires Emacs 27.2 or later (Emacs 29+ recommended for optimal performance due to improved stack frame handling)
- Only depends on built-in `cl-lib` package
- No external dependencies

## Debugging

Enable spy mode to trace predicate execution:
```elisp
(setq eprolog-spy-predicates '(predicate-name))
(setq eprolog-spy-state 'prompt) ; or 'always or 'disabled
```

The spy system provides detailed execution tracing for debugging complex Prolog programs.