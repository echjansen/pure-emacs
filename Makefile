EMACS ?= emacs

all: clean eamcs

clean: ## Remove build artifacts
	rm -rf modules/*.elc

install: ## (re)install Emacs
	rm -rf elpa
	rm -rf eln-cache
	rm -rf modules/*.elc
	rm -rf tree-sitter
	${EMACS} -Q -batch -f batch-byte-compile init.el modules/*.el

emacs: clean
	${EMACS} -Q -batch -f batch-byte-compile init.el modules/*.el

report:
	${EMACS} -Q -batch --debug-init -f batch-byte-compile init.el modules/*.el
	$(EMACS) -nw --debug-init -f use-package-report

.PHONY: all
