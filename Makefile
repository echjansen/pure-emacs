EMACS ?= emacs

all: clean eamcs

clean: ## Remove build artifacts
	rm -rf modules/*.elc
	rm -rf elpa/*/*.elc

emacs: clean
	${EMACS} -Q -batch -f batch-byte-compile modules/*.el
	${EMACS} -nw -f custom-save-all

report: clean
	$(EMACS) -nw --debug-init -f use-package-report

.PHONY: all
