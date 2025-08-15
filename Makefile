EMACS ?= emacs
MAIN_FILE = eprolog.el
USAGE_FILE = BOOK.org
USAGE_EL = BOOK.el

.PHONY: test check clean format help

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

format:
	$(EMACS) -batch \
		--eval "(progn (load \"$(MAIN_FILE)\") (find-file \"$(MAIN_FILE)\") (emacs-lisp-mode) (untabify (point-min) (point-max)) (indent-region (point-min) (point-max)) (delete-trailing-whitespace) (save-buffer))"

clean:
	@rm -f $(USAGE_EL) *.elc

help:
	@echo "Targets: test, check, clean, format, help"
