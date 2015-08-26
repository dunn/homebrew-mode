.PHONY: compile clean major minor patch test

prefix ?= /usr/local
lispdir?= $(prefix)/share/emacs/site-lisp/homebrew-mode

emacs ?= $(shell which emacs)
flags ?= --directory .

BASE_FILE = homebrew-mode.el
LISPS = $(BASE_FILE)

default: compile

compile: $(LISPS)
	$(emacs) --batch -Q $(flags) -f batch-byte-compile $<

install: compile
	mkdir -p $(lispdir)
	install -m 644 $(LISPS) *.elc $(lispdir)

test:
	$(emacs) --batch -Q -l ert -l ./homebrew-mode.el -l ./tests/homebrew-mode-tests.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

clean:
	rm *.elc *~

temp ?= $(uuid)

major:
	HM_VERSION_SHIFT=major build/version.awk $(BASE_FILE) > $(temp).el
	rm $(BASE_FILE)
	mv $(temp).el $(BASE_FILE)

minor:
	HM_VERSION_SHIFT=minor build/version.awk $(BASE_FILE) > $(temp).el
	rm $(BASE_FILE)
	mv $(temp).el $(BASE_FILE)

patch:
	HM_VERSION_SHIFT=patch build/version.awk $(BASE_FILE) > $(temp).el
	rm $(BASE_FILE)
	mv $(temp).el $(BASE_FILE)

version=$(shell ack -o -m 1 "[0-9]+\.[0-9]+\.[0-9]+" $(BASE_FILE))
tag:
	git tag -a $(version) -m "v$(version)"
