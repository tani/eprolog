# Emacs Prolog implementation build configuration
EMACS      ?= emacs
MAIN_FILE   = eprolog.el
# Legacy files removed - now using modular docs/

# Documentation module sources and targets
DOC_SOURCES = $(wildcard docs/*.org)
DOC_TARGETS = $(DOC_SOURCES:.org=.el)

.PHONY: all test check clean format help docs

# Default target
all: test


# Generate documentation modules (index.el first, then others depend on it)
docs/index.el: docs/index.org
	$(EMACS) -batch -l $(MAIN_FILE) \
		--eval '(org-babel-load-file "$<")' 2>/dev/null

docs/%.el: docs/%.org docs/index.el
	$(EMACS) -batch -l $(MAIN_FILE) -l docs/index.el \
		--eval '(org-babel-load-file "$<")' 2>/dev/null

# Run test suite with modular documentation
test: $(DOC_TARGETS)
	$(EMACS) -batch -l $(MAIN_FILE) \
		-l docs/index.el \
		$(foreach doc,$(filter-out docs/index.el,$(DOC_TARGETS)),-l $(doc)) \
		--eval '(ert-run-tests-batch-and-exit "eprolog-")'


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
	@rm -f $(DOC_TARGETS) *.elc docs/*.elc

# Show available targets
help:
	@echo "Available targets:"
	@echo "  all        - Run tests (default)"
	@echo "  test       - Run test suite with modular documentation"
	@echo "  check      - Lint and validate code"
	@echo "  format     - Format source code"
	@echo "  clean      - Remove generated files"
	@echo "  help       - Show this help message"
