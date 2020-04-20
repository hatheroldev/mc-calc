EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} compile
	${MAKE} unittest
	${MAKE} behaviourtest
	${MAKE} clean

.cask:
	${CASK}

compile: .cask
	${CASK} exec ${EMACS} -Q -batch -L . -eval \
	"(progn \
     (when (version<= \"24.3\" emacs-version) \
     (setq byte-compile-error-on-warn t)) \
     (batch-byte-compile))" mc-calc.el

lint: .cask
	${CASK} exec ${EMACS} -Q --batch -l elisp-lint.el -f elisp-lint-files-batch *.el

unittest: .cask
	${CASK} exec ert-runner

behaviourtest: .cask
	${CASK} exec ecukes --quiet

clean:
	rm -f *.elc

.PHONY: all compile unittest behaviourtest clean
