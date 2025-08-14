EMACS ?= emacs
MAIN_FILE = eprolog.el
USAGE_FILE = BOOK.org
USAGE_EL = BOOK.el

.PHONY: test check clean help

all: test

$(USAGE_EL): $(USAGE_FILE)
	$(EMACS) -batch -l $(MAIN_FILE) \
		--eval "(org-babel-load-file \"$(USAGE_FILE)\")" 2>/dev/null

test: $(USAGE_EL)
	$(EMACS) -batch -l $(MAIN_FILE) -l $(USAGE_EL) \
		--eval "(ert-run-tests-batch-and-exit \"eprolog-usage-\")"

check:
	$(EMACS) -batch --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(MAIN_FILE)
	@rm -f $(MAIN_FILE)c

clean:
	@rm -f $(USAGE_EL) *.elc

help:
	@echo "Targets: test, check, clean, help"
