USER=dlichteblau

all: index.html installation.html hax.html examples.html

%.html: %.xml index.xsl
	xsltproc index.xsl $< >$@.tmp
	mv -f $@.tmp $@
	chmod -w $@

.PHONY: push
push: all
	scp index.html index.css $(USER)@common-lisp.net:/project/closure/public_html/closure-html/
