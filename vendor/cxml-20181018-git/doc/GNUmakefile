all: dom.html index.html installation.html klacks.html quickstart.html sax.html xmls-compat.html

%.html: %.xml html.xsl
	xsltproc html.xsl $< >$@.tmp
	mv -f $@.tmp $@
	chmod -w $@
