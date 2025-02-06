EMACS ?= emacs

all: clean eamcs

clean: ## Remove build artifacts
	rm -rf modules/*.elc

install: ## (re)install Emacs
	rm -rf elpa
	rm -rf eln-cache
	rm -rf modules/*.elc
	rm -rf tree-sitter
	${EMACS} -Q -batch -f batch-byte-compile init.el modules/pure-common.el modules/pure-emacs.el modules/pure-future.el

emacs: clean
	${EMACS} -Q -batch -f batch-byte-compile init.el modules/pure-common.el modules/pure-emacs.el modules/pure-future.el

report:
	$(EMACS) -nw --debug-init -f use-package-report

profile:
	emacs -Q -l profile-emacs.el -f profile-dotemacs

.PHONY: all
