# Makefile for ε-prolog
# Provides convenient targets for testing, linting, and development

# Emacs executable
EMACS ?= emacs

# Source files
MAIN_FILE = eprolog.el
USAGE_FILE = BOOK.org
USAGE_EL = BOOK.el

# Test targets
.PHONY: test test-usage clean help

# Default target
all: test

# Run usage examples as tests
test-usage: $(USAGE_EL)
	@echo "Running usage example tests..."
	$(EMACS) -batch -l $(MAIN_FILE) -l $(USAGE_EL) \
		--eval "(ert-run-tests-batch-and-exit \"eprolog-usage-\")"

# Generate BOOK.el from BOOK.org if needed
$(USAGE_EL): $(USAGE_FILE)
	@echo "Generating BOOK.el from BOOK.org..."
	$(EMACS) -batch -l $(MAIN_FILE) \
		--eval "(org-babel-load-file \"$(USAGE_FILE)\")" \
		--eval "(message \"Generated $(USAGE_EL) successfully\")" 2>/dev/null

# Default test target (runs usage tests which now contain all tests)
test: test-usage

# Interactive test running
test-interactive: $(USAGE_EL)
	@echo "Starting interactive test session..."
	$(EMACS) -l $(MAIN_FILE) -l $(USAGE_EL) \
		--eval "(ert-run-tests-interactively \"eprolog-usage-\")"

# Syntax check
check:
	@echo "Checking syntax..."
	$(EMACS) -batch --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(MAIN_FILE)
	@rm -f $(MAIN_FILE)c

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f $(USAGE_EL) *.elc

# Development helpers
dev-setup:
	@echo "Development setup complete."
	@echo "Available targets: test, test-usage, check, clean"

# Performance test (run with timing)
perf-test: $(USAGE_EL)
	@echo "Running performance tests..."
	@time $(EMACS) -batch -l $(MAIN_FILE) -l $(USAGE_EL) \
		--eval "(ert-run-tests-batch-and-exit \"eprolog-usage-\")"

# Verbose test output
test-verbose: $(USAGE_EL)
	@echo "Running tests with verbose output..."
	$(EMACS) -batch -l $(MAIN_FILE) -l $(USAGE_EL) \
		--eval "(ert-run-tests-batch \"eprolog-usage-\")"

# Count test statistics  
stats: $(USAGE_EL)
	@echo "=== Test Statistics ==="
	@echo "Usage tests: $(shell grep -c 'ert-deftest eprolog-usage-' $(USAGE_EL))"
	@echo "Total lines in $(MAIN_FILE): $(shell wc -l < $(MAIN_FILE))"
	@echo "DCG predicates: $(shell grep -c 'eprolog-define-grammar' $(MAIN_FILE))"
	@echo "Built-in predicates: $(shell grep -c 'eprolog-define-lisp-predicate' $(MAIN_FILE))"

# Help target
help:
	@echo "ε-prolog Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  test          - Run test suite (default)"
	@echo "  test-usage    - Run usage example tests (same as test)"
	@echo "  test-interactive - Run tests interactively in Emacs"
	@echo "  test-verbose  - Run tests with verbose output"
	@echo "  perf-test     - Run tests with timing information"
	@echo "  check         - Check syntax and byte-compile"
	@echo "  stats         - Show test and code statistics"
	@echo "  clean         - Remove generated files"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS         - Emacs executable (default: emacs)"
	@echo ""
	@echo "Examples:"
	@echo "  make test                    # Run tests"
	@echo "  make test-usage             # Run tests (same as above)"
	@echo "  make EMACS=/usr/bin/emacs28 test  # Use specific Emacs version"
