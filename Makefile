.PHONY: default compile clean major minor patch tag test

prefix ?= /usr/local
lispdir?= $(prefix)/share/emacs/site-lisp/homebrew-mode

emacs    ?= $(shell which emacs)
inf_ruby ?= /usr/local/share/emacs/site-lisp/inf-ruby/
dash     ?= /usr/local/share/emacs/site-lisp/dash/

BASE_FILE = homebrew-mode.el
LISPS = $(BASE_FILE)

default: compile

compile: $(LISPS)
	$(emacs) --batch -Q --directory $(inf_ruby) --directory $(dash) -f batch-byte-compile $<

install: compile
	mkdir -p $(lispdir)
	install -m 644 $(LISPS) *.elc $(lispdir)

test:
	$(emacs) --batch -Q --load ert --directory $(inf_ruby) --directory $(dash) \
		--load ./homebrew-mode.el --load ./tests/homebrew-ert-tests.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

clean:
	rm *.elc

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
