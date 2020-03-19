EMACS = emacs

tests:
	$(EMACS) --script t/divine-core-tests.el
