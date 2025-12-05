EMACS ?= emacs

all: clean eamcs

clean: ## Remove build artifacts
	rm -rf init.elc
	rm -rf modules/*.elc

install: clean ## (re)install Emacs
	rm -rf elpa
	rm -rf eln-cache
	${EMACS} -Q -batch -f batch-byte-compile init.el modules/*.el
	rm init.elc

emacs: clean
	${EMACS} -Q -batch -f batch-byte-compile init.el modules/*.el
	rm init.elc

report:
	${EMACS} -Q -batch --debug-init -f batch-byte-compile init.el modules/*.el
	rm init.elc
	$(EMACS) -nw --debug-init -f use-package-report

.PHONY: all
