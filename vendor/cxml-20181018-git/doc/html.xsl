<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>	
  </xsl:template>

  <xsl:template match="documentation">
    <html>
      <head>
	<title>
	  <xsl:value-of select="@title"/>
	</title>
	<link rel="stylesheet" type="text/css" href="cxml.css"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body>
	<div class="sidebar">
	  <div class="sidebar-title">
	    <a href="index.html">Closure XML</a>
	  </div>
	  <div class="sidebar-main">
	    <ul class="main">
	      <li>
		<a href="installation.html">Installing Closure XML</a>
		<ul class="sub">
		  <li><a href="installation.html#download"><b>Download</b></a></li>
		  <li><a href="installation.html#implementations">Implementation-specific notes</a></li>
		  <li><a href="installation.html#compilation">Compilation</a></li>
		  <li><a href="installation.html#tests">Tests</a></li>
		</ul>
	      </li>
              <li>
		<ul class="hack">
		  <li>
		    <a href="quickstart.html"><b>Quick-Start Example / FAQ</b></a>
		  </li>
		</ul>
              </li>
	      <li>
		<a href="sax.html">SAX parsing and serialization</a>
		<ul class="sub">
		  <li><a href="sax.html#parser">Parsing and Validating</a></li>
		  <li><a href="sax.html#serialization">Serialization</a></li>
		  <li><a href="sax.html#misc">Miscellaneous SAX handlers</a></li>
		  <li><a href="sax.html#rods">Recoders</a></li>
		  <li><a href="sax.html#dtdcache">Caching of DTD Objects</a></li>
		  <li><a href="sax.html#catalogs">XML Catalogs</a></li>
		  <li><a href="sax.html#sax">SAX Interface</a></li>
		</ul>
	      </li>
	      <li>
		<a href="klacks.html">Klacks parser</a>
		<ul class="sub">
		  <li><a href="klacks.html#sources">Parsing incrementally</a></li>
		  <li><a href="klacks.html#convenience">Convenience functions</a></li>
		  <li><a href="klacks.html#klacksax">Bridging Klacks and SAX</a></li>
		  <li><a href="klacks.html#locator">Location information</a></li>
		  <li><a href="klacks.html#klacksax">Examples</a></li>
		</ul>
              </li>
	      <li>
		<a href="dom.html">DOM implementation</a>
		<ul class="sub">
		  <li><a href="dom.html#parser">Parsing with the DOM builder</a></li>
		  <li><a href="dom.html#serialization">Serialization</a></li>
		  <li><a href="dom.html#mapping">DOM/Lisp mapping</a></li>
		</ul>
	      </li>
	      <li>
		<ul class="hack">
		  <li><a href="xmls-compat.html">XMLS Builder</a></li>
		</ul>
              </li>
	    </ul>
	  </div>
	</div>
	  <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="page-index">
    <ul>
      <xsl:for-each select="//heading">
	<li>
	  <a href="#{generate-id()}">
	    <xsl:copy>
	      <xsl:apply-templates select="node()"/>
	    </xsl:copy>
	  </a>
	</li>
      </xsl:for-each>
    </ul>
  </xsl:template>

  <xsl:template match="heading">
    <a name="{generate-id()}"/>
    <h3>
      <xsl:copy>
	<xsl:apply-templates select="node()"/>
      </xsl:copy>	
    </h3>
  </xsl:template>
</xsl:stylesheet>
