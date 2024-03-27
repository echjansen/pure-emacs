EMACS ?= emacs

all: clean compile

clean: ## Remove build artifacts
	rm -f modules/*.elc

compile: clean
	${EMACS} -Q -batch -f batch-byte-compile modules/*.el

.PHONY: all clean compile
