.PHONY: all test clean
EMACS ?= emacs
CASK ?= cask

test:
	${MAKE} unit-test
	${MAKE} inte-test

unit-test:
	${CASK} exec ert-runner


inte-test:
	${CASK} exec ecukes
