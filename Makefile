EMACS=emacs

ORG_ARGS  = --batch
ORG_ARGS += --dir .
ORG_ARGS += --file divine.org
ORG_ARGS += -l divine -l ox-texinfo+
ORG_ARGS += --funcall org-texinfo-export-to-texinfo
ORG_ARGS += --funcall org-babel-tangle

.PHONY: clean tests

all: divine.info divine/index.html

info : divine.info

divine/index.html : divine.texi manual.css
	makeinfo --html --output=web --css-include manual.css divine.texi

divine.texi manual.css : divine.org ox-texinfo+.el
	$(EMACS) $(ORG_ARGS)
#	$(EMACS) --batch --dir . -l ox -l divine --file divine.org -f org-texinfo-export-to-texinfo -f org-babel-tangle

tests:
	$(EMACS) --batch --dir . -l ert -l t/divine-core-tests.el -f ert-run-tests-batch-and-exit

ox-texinfo+.el :
	curl https://raw.githubusercontent.com/tarsius/ox-texinfo-plus/master/ox-texinfo%2B.el > ox-texinfo+.el

clean:
	$(RM) divine.texi

%.texi : %.org ;
