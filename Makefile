EMACS ?= emacs

all: clean eamcs

clean: ## Remove build artifacts
	rm -rf modules/*.elc

install: ## (re)install Emacs
	rm -rf elpa
	rm -rf eln-cache
	rm -rf modules/*.elc
	${EMACS} -Q -batch -f batch-byte-compile modules/*.el

emacs: clean
	${EMACS} -Q -batch -f batch-byte-compile modules/*.el

report: clean
	$(EMACS) -nw --debug-init -f use-package-report

.PHONY: all
