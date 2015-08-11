.PHONY: compile clean major minor patch

emacs ?= $(shell which emacs)
flags ?= --directory .

LISPS = homebrew-mode.el

default: compile

compile: $(LISPS)
	$(emacs) --batch -Q $(flags) -f batch-byte-compile $<

clean:
	rm *.elc *~

temp ?= $(uuid)

major:
	HM_VERSION_SHIFT=major build/version.awk homebrew-mode.el > $(uuid).el
	rm homebrew-mode.el
	mv $(uuid).el homebrew-mode.el

minor:
	HM_VERSION_SHIFT=minor build/version.awk homebrew-mode.el > $(uuid).el
	rm homebrew-mode.el
	mv $(uuid).el homebrew-mode.el

patch:
	HM_VERSION_SHIFT=patch build/version.awk homebrew-mode.el > $(uuid).el
	rm homebrew-mode.el
	mv $(uuid).el homebrew-mode.el
