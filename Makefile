.PHONY: compile clean major minor patch

emacs ?= $(shell which emacs)
flags ?= --directory .

LISPS = homebrew-mode.el

default: compile

compile: $(LISPS)
	$(emacs) --batch -Q $(flags) -f batch-byte-compile $<

clean:
	rm *.elc *~
