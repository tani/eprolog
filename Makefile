# Emacs Prolog implementation build configuration
EMACS      ?= emacs
MAIN_FILE   = eprolog.el
USAGE_FILE  = BOOK.org
USAGE_EL    = BOOK.el

.PHONY: all test check clean format help

# Default target
all: test

# Generate usage examples from documentation
$(USAGE_EL): $(USAGE_FILE)
	$(EMACS) -batch -l $(MAIN_FILE) \
		--eval '(org-babel-load-file "$(USAGE_FILE)")' 2>/dev/null

# Run test suite
test: $(USAGE_EL)
	$(EMACS) -batch -l $(MAIN_FILE) -l $(USAGE_EL) \
		--eval '(ert-run-tests-batch-and-exit "eprolog-usage-")'

# Lint and validate code
check:
	@echo "Running byte compilation check..."
	$(EMACS) -batch \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(MAIN_FILE)
	rm -f "$(MAIN_FILE)c"
	@echo "Running documentation check..."
	$(EMACS) -batch \
		--eval '(checkdoc-file "$(MAIN_FILE)")'

# Format source code
format:
	@echo "Formatting $(MAIN_FILE)..."
	$(EMACS) -batch \
		--eval '(load "$(MAIN_FILE)")' \
		--eval '(find-file "$(MAIN_FILE)")' \
		--eval '(emacs-lisp-mode)' \
		--eval '(untabify (point-min) (point-max))' \
		--eval '(indent-region (point-min) (point-max))' \
		--eval '(delete-trailing-whitespace)' \
		--eval '(save-buffer)'

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f $(USAGE_EL) *.elc

# Show available targets
help:
	@echo "Available targets:"
	@echo "  all     - Run tests (default)"
	@echo "  test    - Run test suite"
	@echo "  check   - Lint and validate code"
	@echo "  format  - Format source code"
	@echo "  clean   - Remove generated files"
	@echo "  help    - Show this help message"
