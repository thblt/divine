EMACS = emacs
SASS = sass

SASS_ARGS = --sourcemap=none

ORG_ARGS  = --batch
ORG_ARGS += --dir .
ORG_ARGS += --file divine.org
ORG_ARGS += -l divine -l ox-texinfo+
ORG_ARGS += --funcall org-texinfo-export-to-texinfo
ORG_ARGS += --funcall org-babel-tangle

.PHONY: clean tests

all: divine.info doc_html

info : divine.info

doc_html: doc_html/index.html doc_html/divine.html doc_html/divine.css

doc_html/index.html : divine.texi
	makeinfo --html --output=doc_html --css-ref divine.css divine.texi

doc_html/divine.html : divine.texi
	makeinfo --html --output=doc_html --no-split --css-ref divine.css divine.texi

doc_html/divine.css : divine.scss
	$(SASS) $(SASS_ARGS) $< $@

divine.texi: divine.org ox-texinfo+.el
	$(EMACS) $(ORG_ARGS)
#	$(EMACS) --batch --dir . -l ox -l divine --file divine.org -f org-texinfo-export-to-texinfo -f org-babel-tangle

tests:
	$(EMACS) --batch --dir . -l ert -l t/divine-core-tests.el -f ert-run-tests-batch-and-exit

ox-texinfo+.el :
	curl https://raw.githubusercontent.com/tarsius/ox-texinfo-plus/master/ox-texinfo%2B.el > ox-texinfo+.el

clean:
	$(RM) divine.info doc_html ox-texinfo+.el

%.texi : %.org ;
