(in-package :documentation)

(define-variable *style*
    "
div.head h1 {
    margin-bottom: 0.8em;
}

dl dd {
    margin: .5em 0 .5em 2em;
}

#github-button {
    vertical-align: middle;
    margin: 0 10px;
    position: relative;
    top: 2px;
}

#github-button a {
    color: transparent;
    text-decoration: none;
    border-bottom: none;
}

.twitter-follow-button {
    vertical-align: middle;
    margin: 0 10px;

    color: transparent !important;
    text-decoration: none !important;
    border-bottom: none !important;
}

.symbol-type {
    font-size: x-small;
}

.evaluates-to {
    margin: 0 1em;
}

dl.function-arguments {
    margin: 0;
}

.function-arguments dt {
    display: inline;
    font-weight: normal;
    font-style: italic;
    text-transform: lowercase;
}

.function-arguments dd {
    display: inline;
    margin: .5em 0 .5em 2em;
}

table.symbol-table {
    background: unset;
    border-left-color: transparent;
    // border-left-color: silver;
    // background: #eee;
}

.package-name,
.superclass-package-name {
    font-style: italic;
}

.symbol-colon {
    margin: 0 0.2em;
}

/* .symbol { */
/*     padding: .1em .5em .15em; */
/*     border-color: #52E052; */
/*     background: #E9FBE9; */
/*     overflow: auto; */
/*     margin: 1em; */
/*     border: .5em; */
/*     border-left-style: solid; */
/*     page-break-inside: avoid; */
/* } */

.heading, .issue, .note, .example, li, dt {
    position: relative;
}
a.self-link {
    position: absolute;
    top: 0;
    left: calc(-1 * (3.5rem - 26px));
    width: calc(3.5rem - 26px);
    height: 100%;
    text-align: center;
    border: none;
    transition: opacity .2s;
    opacity: .5;
    line-height: 1.2;
}
a.self-link:hover {
    opacity: 1;
}
.heading > a.self-link {

}
li > a.self-link {
    left: calc(-1 * (3.5rem - 26px) - 2em);
}
dfn > a.self-link {
    top: auto;
    left: auto;
    opacity: 0;
    width: 1.5em;
    height: 1.5em;
    background: gray;
    color: white;
    font-style: normal;
    transition: opacity .2s, background-color .2s, color .2s;
}
dfn:hover > a.self-link {
    opacity: 1;
}
dfn > a.self-link:hover {
    color: black;
}

a.self-link::before            { content: \"¶\"; }
.heading > a.self-link::before { content: \"§\"; }
dfn > a.self-link::before      { content: \"#\"; }

")

(define-variable *default-style*
    "
/*
 * Style sheet for the CSS3 specification,
 * to be used in addition to https://www.w3.org/StyleSheets/TR/W3C-{WD,PR,REC}
 */



/** Example Code **************************************************************/

	div.html, div.xml,
	pre.html, pre.xml {
		padding: 0.5em;
		margin: 1em 0;
		position: relative;
		clear: both;
		color: #600;
	}
	pre.example,
	pre.html,
	pre.xml {
		padding-top: 1.5em;
	}

	pre.illegal, .illegal pre
	pre.deprecated, .deprecated pre {
		color: red;
	}

/** Inline markup fragments ***************************************************/

	code.html, code.xml {
		color: #660000;
	}

/******************************************************************************/
/*                                    Images                                  */
/******************************************************************************/

	pre.ascii-art {
		display: table; /* shrinkwrap */
		margin: 1em auto;
		line-height: normal;
	}

	/* Displaying the output of text layout, particularly when
		 line-breaking is being invoked, and thus having a
		 visible width is good. */
	pre.output {
		margin: 1em;
		border: solid thin silver;
		padding: 0.25em;
		display: table;
	}

/******************************************************************************/
/*                                    Tables                                  */
/******************************************************************************/

/* XXX: Remove these once all specs are bikeshedded or Bert's processor generates .def/.index classes */

/** Property/Descriptor Definition Tables *************************************/

	table.propdef, table.propdef-extra,
	table.descdef, table.definition-table {
		page-break-inside: avoid;
		width: 100%;
		margin: 1.2em 0;
		border-left: 0.5em solid #8CCBF2;
		padding: 0.5em 1em;
		background: #DEF;
		border-spacing: 0;
	}

	table.propdef td, table.propdef-extra td,
	table.descdef td, table.definition-table td,
	table.propdef th, table.propdef-extra th,
	table.descdef th, table.definition-table th {
		padding: 0.5em;
		vertical-align: baseline;
		border-bottom: 1px solid #bbd7e9;
	}
	table.propdef > tbody > tr:last-child th,
	table.propdef-extra > tbody > tr:last-child th,
	table.descdef > tbody > tr:last-child th,
	table.definition-table > tbody > tr:last-child th,
	table.propdef > tbody > tr:last-child td,
	table.propdef-extra > tbody > tr:last-child td,
	table.descdef > tbody > tr:last-child td,
	table.definition-table > tbody > tr:last-child td {
		border-bottom: 0;
	}

	table.propdef th,
	table.propdef-extra th,
	table.descdef th,
	table.definition-table th {
		font-style: italic;
		font-weight: normal;
		width: 8.3em;
		padding-left: 1em;
	}

	/* For when values are extra-complex and need formatting for readability */
	table td.pre {
		white-space: pre-wrap;
	}

	/* A footnote at the bottom of a propdef */
	table.propdef td.footnote,
	table.propdef-extra td.footnote,
	table.descdef td.footnote,
	table.definition-table td.footnote {
		padding-top: 0.6em;
	}
	table.propdef td.footnote::before,
	table.propdef-extra td.footnote::before,
	table.descdef td.footnote::before,
	table.definition-table td.footnote::before {
		content: \" \";
		display: block;
		height: 0.6em;
		width: 4em;
		border-top: thin solid;
	}

	.all-media {
		font-style: italic;
	}

/** Profile Tables ************************************************************/
	/* table of required features in a CSS profile */

	table.features th {
		background: #00589f;
		color: #fff;
		text-align: left;
		padding: 0.2em 0.2em 0.2em 0.5em;
	}
	table.features td {
		vertical-align: top;
		border-bottom: 1px solid #ccc;
		padding: 0.3em 0.3em 0.3em 0.7em;
	}

/** At-risk List **************************************************************/
	/* Style for At Risk features (intended as editorial aid, not intended for publishing) */

	.atrisk::before {
	 float: right;
	 margin-top: -0.25em;
	 padding: 0.5em 1em 0.5em 0;
	 text-indent: -0.9em;
	 border: 1px solid;
	 content: '\25C0    Not yet widely implemented';
	 white-space: pre;
	 font-size: small;
	 background-color: white;
	 color: gray;
	 text-align: center;
	}

	.toc .atrisk::before { content:none }

/** File Issue Link ***********************************************************/
	/* Style for https://resources.whatwg.org/file-issue.js */

	.selected-text-file-an-issue {
	 position: fixed;
	 bottom: 0;
	 right: 0;
	 background: rgba(255, 255, 255, 0.8);
	 font-size: smaller;
	 padding: 4px 10px;
	 z-index: 1;
	 text-decoration: underline;
	}
	@media (max-width: 767px) {
	 .selected-text-file-an-issue { left: 0; right: auto; text-align: left; }
	}
")

(define-variable *base-style*
    "
/******************************************************************************
 *                   Style sheet for the W3C specifications                   *
 *
 * Special classes handled by this style sheet include:
 *
 * Indices
 *   - .toc for the Table of Contents (<ol class=\"toc\">)
 *     + <span class=\"secno\"> for the section numbers
 *   - #toc for the Table of Contents (<nav id=\"toc\">)
 *   - ul.index for Indices (<a href=\"#ref\">term</a><span>, in §N.M</span>)
 *   - table.index for Index Tables (e.g. for properties or elements)
 *
 * Structural Markup
 *   - table.data for general data tables
 *     -> use 'scope' attribute, <colgroup>, <thead>, and <tbody> for best results !
 *     -> use <table class='complex data'> for extra-complex tables
 *     -> use <td class='long'> for paragraph-length cell content
 *     -> use <td class='pre'> when manual line breaks/indentation would help readability
 *   - dl.switch for switch statements
 *   - ol.algorithm for algorithms (helps to visualize nesting)
 *   - .figure and .caption (HTML4) and figure and figcaption (HTML5)
 *     -> .sidefigure for right-floated figures
 *   - ins/del
 *
 * Code
 *   - pre and code
 *
 * Special Sections
 *   - .note       for informative notes             (div, p, span, aside, details)
 *   - .example    for informative examples          (div, p, pre, span)
 *   - .issue      for issues                        (div, p, span)
 *   - .advisement for loud normative statements     (div, p, strong)
 *   - .annoying-warning for spec obsoletion notices (div, aside, details)
 *
 * Definition Boxes
 *   - pre.def   for WebIDL definitions
 *   - table.def for tables that define other entities (e.g. CSS properties)
 *   - dl.def    for definition lists that define other entitles (e.g. HTML elements)
 *
 * Numbering
 *   - .secno for section numbers in .toc and headings (<span class='secno'>3.2</span>)
 *   - .marker for source-inserted example/figure/issue numbers (<span class='marker'>Issue 4</span>)
 *   - ::before styled for CSS-generated issue/example/figure numbers:
 *     -> Documents wishing to use this only need to add
 *        figcaption::before,
 *        .caption::before { content: \"Figure \"  counter(figure) \" \";  }
 *        .example::before { content: \"Example \" counter(example) \" \"; }
 *        .issue::before   { content: \"Issue \"   counter(issue) \" \";   }
 *
 * Header Stuff (ignore, just don't conflict with these classes)
 *   - .head for the header
 *   - .copyright for the copyright
 *
 * Outdated warning for old specs
 *
 * Miscellaneous
 *   - .overlarge for things that should be as wide as possible, even if
 *     that overflows the body text area. This can be used on an item or
 *     on its container, depending on the effect desired.
 *     Note that this styling basically doesn't help at all when printing,
 *     since A4 paper isn't much wider than the max-width here.
 *     It's better to design things to fit into a narrower measure if possible.
 *   - js-added ToC jump links (see fixup.js)
 *
 ******************************************************************************/

/******************************************************************************/
/*                                   Body                                     */
/******************************************************************************/

	body {
		counter-reset: example figure issue;

		/* Layout */
		max-width: 50em;               /* limit line length to 50em for readability   */
		margin: 0 auto;                /* center text within page                     */
		padding: 1.6em 1.5em 2em 50px; /* assume 16px font size for downlevel clients */
		padding: 1.6em 1.5em 2em calc(26px + 1.5em); /* leave space for status flag     */

		/* Typography */
		line-height: 1.5;
		font-family: sans-serif;
		widows: 2;
		orphans: 2;
		word-wrap: break-word;
		overflow-wrap: break-word;

		/* Colors */
		color: black;
		background: white top left fixed no-repeat;
		background-size: 25px auto;
	}


/******************************************************************************/
/*                         Front Matter & Navigation                          */
/******************************************************************************/

/** Header ********************************************************************/

	div.head { margin-bottom: 1em; }
	div.head hr { border-style: solid; }

	div.head h1 {
		font-weight: bold;
		margin: 0 0 .1em;
		font-size: 220%;
	}

	div.head h2 { margin-bottom: 1.5em;}

/** W3C Logo ******************************************************************/

	.head p:not(.copyright):first-child {
		margin: 0;
	}

	.head p:not(.copyright):first-child > a,
	.head > a:first-child {
		float: right;
		margin: 0.4rem 0 0.2rem .4rem;
	}

	.head img[src*=\"logos/W3C\"] {
		display: block;
		border: solid #1a5e9a;
		border-width: .65rem .7rem .6rem;
		border-radius: .4rem;
		background: #1a5e9a;
		color: white;
		font-weight: bold;
	}

	.head a:hover > img[src*=\"logos/W3C\"],
	.head a:focus > img[src*=\"logos/W3C\"] {
		opacity: .8;
	}

	.head a:active > img[src*=\"logos/W3C\"] {
		background: #c00;
		border-color: #c00;
	}

	/* see also additional rules in Link Styling section */

/** Copyright *****************************************************************/

	p.copyright,
	p.copyright small { font-size: small; }

/** Back to Top / ToC Toggle **************************************************/

	@media print {
		#toc-nav {
			display: none;
		}
	}
	@media not print {
		#toc-nav {
			position: fixed;
			z-index: 2;
			bottom: 0; left: 0;
			margin: 0;
			min-width: 1.33em;
			border-top-right-radius: 2rem;
			box-shadow: 0 0 2px;
			font-size: 1.5em;
			color: black;
		}
		#toc-nav > a {
			display: block;
			white-space: nowrap;

			height: 1.33em;
			padding: .1em 0.3em;
			margin: 0;

			box-shadow: 0 0 2px;
			border: none;
			border-top-right-radius: 1.33em;
			background: white;
		}
		#toc-nav > #toc-jump {
			padding-bottom: 2em;
			margin-bottom: -1.9em;
		}

		#toc-nav > a:hover,
		#toc-nav > a:focus {
			background: #f8f8f8;
		}
		#toc-nav > a:not(:hover):not(:focus) {
			color: #707070;
		}

		/* statusbar gets in the way on keyboard focus; remove once browsers fix */
		#toc-nav > a[href=\"#toc\"]:not(:hover):focus:last-child {
			padding-bottom: 1.5rem;
		}

		#toc-nav:not(:hover) > a:not(:focus) > span + span {
			/* Ideally this uses :focus-within on #toc-nav */
			display: none;
		}
		#toc-nav > a > span + span {
			padding-right: 0.2em;
		}

		#toc-toggle-inline {
			vertical-align: 0.05em;
			font-size: 80%;
			color: gray;
			color: hsla(203,20%,40%,.7);
			border-style: none;
			background: transparent;
			position: relative;
		}
		#toc-toggle-inline:hover:not(:active),
		#toc-toggle-inline:focus:not(:active) {
			text-shadow: 1px 1px silver;
			top: -1px;
			left: -1px;
		}

		#toc-nav :active {
			color: #C00;
		}
	}

/** ToC Sidebar ***************************************************************/

	/* Floating sidebar */
	@media screen {
		body.toc-sidebar #toc {
			position: fixed;
			top: 0; bottom: 0;
			left: 0;
			width: 23.5em;
			max-width: 80%;
			max-width: calc(100% - 2em - 26px);
			overflow: auto;
			padding: 0 1em;
			padding-left: 42px;
			padding-left: calc(1em + 26px);
			background: inherit;
			background-color: #f7f8f9;
			z-index: 1;
			box-shadow: -.1em 0 .25em rgba(0,0,0,.1) inset;
		}
		body.toc-sidebar #toc h2 {
			margin-top: .8rem;
			font-variant: small-caps;
			font-variant: all-small-caps;
			text-transform: lowercase;
			font-weight: bold;
			color: gray;
			color: hsla(203,20%,40%,.7);
		}
		body.toc-sidebar #toc-jump:not(:focus) {
			width: 0;
			height: 0;
			padding: 0;
			position: absolute;
			overflow: hidden;
		}
	}
	/* Hide main scroller when only the ToC is visible anyway */
	@media screen and (max-width: 28em) {
		body.toc-sidebar {
			overflow: hidden;
		}
	}

	/* Sidebar with its own space */
	@media screen and (min-width: 78em) {
		body:not(.toc-inline) #toc {
			position: fixed;
			top: 0; bottom: 0;
			left: 0;
			width: 23.5em;
			overflow: auto;
			padding: 0 1em;
			padding-left: 42px;
			padding-left: calc(1em + 26px);
			background: inherit;
			background-color: #f7f8f9;
			z-index: 1;
			box-shadow: -.1em 0 .25em rgba(0,0,0,.1) inset;
		}
		body:not(.toc-inline) #toc h2 {
			margin-top: .8rem;
			font-variant: small-caps;
			font-variant: all-small-caps;
			text-transform: lowercase;
			font-weight: bold;
			color: gray;
			color: hsla(203,20%,40%,.7);
		}

		body:not(.toc-inline) {
			padding-left: 29em;
		}
		/* See also Overflow section at the bottom */

		body:not(.toc-inline) #toc-jump:not(:focus) {
			width: 0;
			height: 0;
			padding: 0;
			position: absolute;
			overflow: hidden;
		}
	}
	@media screen and (min-width: 90em) {
		body:not(.toc-inline) {
			margin: 0 4em;
		}
	}

/******************************************************************************/
/*                                Sectioning                                  */
/******************************************************************************/

/** Headings ******************************************************************/

	h1, h2, h3, h4, h5, h6, dt {
		page-break-after: avoid;
		page-break-inside: avoid;
		font: 100% sans-serif;   /* Reset all font styling to clear out UA styles */
		font-family: inherit;    /* Inherit the font family. */
		line-height: 1.2;        /* Keep wrapped headings compact */
		hyphens: manual;         /* Hyphenated headings look weird */
	}

	h2, h3, h4, h5, h6 {
		margin-top: 3rem;
	}

	h1, h2, h3 {
		color: #005A9C;
		background: transparent;
	}

	h1 { font-size: 170%; }
	h2 { font-size: 140%; }
	h3 { font-size: 120%; }
	h4 { font-weight: bold; }
	h5 { font-style: italic; }
	h6 { font-variant: small-caps; }
	dt { font-weight: bold; }

/** Subheadings ***************************************************************/

	h1 + h2,
	#subtitle {
		/* #subtitle is a subtitle in an H2 under the H1 */
		margin-top: 0;
	}
	h2 + h3,
	h3 + h4,
	h4 + h5,
	h5 + h6 {
		margin-top: 1.2em; /* = 1 x line-height */
	}

/** Section divider ***********************************************************/

	:not(.head) > :not(.head) + hr {
		font-size: 1.5em;
		text-align: center;
		margin: 1em auto;
		height: auto;
		border: transparent solid 0;
		background: transparent;
	}
	:not(.head) > hr::before {
		content: \"\2727\2003\2003\2727\2003\2003\2727\";
	}

/******************************************************************************/
/*                            Paragraphs and Lists                            */
/******************************************************************************/

	p {
		margin: 1em 0;
	}

	dd > p:first-child,
	li > p:first-child {
		margin-top: 0;
	}

	ul, ol {
		margin-left: 0;
		padding-left: 2em;
	}

	li {
		margin: 0.25em 0 0.5em;
		padding: 0;
	}

	dl dd {
		margin: 0 0 .5em 2em;
	}

	.head dd + dd { /* compact for header */
		margin-top: -.5em;
	}

	/* Style for algorithms */
	ol.algorithm ol:not(.algorithm),
	.algorithm > ol ol:not(.algorithm) {
	 border-left: 0.5em solid #DEF;
	}

	/* Style for switch/case <dl>s */
	dl.switch > dd > ol.only {
	 margin-left: 0;
	}
	dl.switch > dd > ol.algorithm {
	 margin-left: -2em;
	}
	dl.switch {
	 padding-left: 2em;
	}
	dl.switch > dt {
	 text-indent: -1.5em;
	 margin-top: 1em;
	}
	dl.switch > dt + dt {
	 margin-top: 0;
	}
	dl.switch > dt::before {
	 content: '\21AA';
	 padding: 0 0.5em 0 0;
	 display: inline-block;
	 width: 1em;
	 text-align: right;
	 line-height: 0.5em;
	}

/** Terminology Markup ********************************************************/


/******************************************************************************/
/*                                 Inline Markup                              */
/******************************************************************************/

/** Terminology Markup ********************************************************/
	dfn   { /* Defining instance */
		font-weight: bolder;
	}
	a > i { /* Instance of term */
		font-style: normal;
	}
	dt dfn code, code.idl {
		font-size: inherit;
	}
	dfn var {
		font-style: normal;
	}

/** Change Marking ************************************************************/

	del { color: red;  text-decoration: line-through; }
	ins { color: #080; text-decoration: underline;    }

/** Miscellaneous improvements to inline formatting ***************************/

	sup {
		vertical-align: super;
		font-size: 80%;
	}

/******************************************************************************/
/*                                    Code                                    */
/******************************************************************************/

/** General monospace/pre rules ***********************************************/

	pre, code, samp {
		font-family: Menlo, Consolas, \"DejaVu Sans Mono\", Monaco, monospace;
		font-size: .9em;
		hyphens: none;
		text-transform: none;
		text-align: left;
		text-align: start;
		font-variant: normal;
		orphans: 3;
		widows: 3;
		page-break-before: avoid;
	}
	pre code,
	code code {
		font-size: 100%;
	}

	pre {
		margin-top: 1em;
		margin-bottom: 1em;
		overflow: auto;
	}

/** Inline Code fragments *****************************************************/

  /* Do something nice. */

/******************************************************************************/
/*                                    Links                                   */
/******************************************************************************/

/** General Hyperlinks ********************************************************/

	/* We hyperlink a lot, so make it less intrusive */
	a[href] {
		color: #034575;
		text-decoration: none;
		border-bottom: 1px solid #707070;
		/* Need a bit of extending for it to look okay */
		padding: 0 1px 0;
		margin: 0 -1px 0;
	}
	a:visited {
		border-bottom-color: #BBB;
	}

	/* Use distinguishing colors when user is interacting with the link */
	a[href]:focus,
	a[href]:hover {
		background: #f8f8f8;
		background: rgba(75%, 75%, 75%, .25);
		border-bottom-width: 3px;
		margin-bottom: -2px;
	}
	a[href]:active {
		color: #C00;
		border-color: #C00;
	}

	/* Backout above styling for W3C logo */
	.head p:not(.copyright) > a,
	.head > a:first-child {
		border: none;
		text-decoration: none;
		background: transparent;
	}

/******************************************************************************/
/*                                    Images                                  */
/******************************************************************************/

	img {
		border-style: none;
	}

	/* For autogen numbers, add
	   .caption::before, figcaption::before { content: \"Figure \" counter(figure) \". \"; }
	*/

	figure, .figure, .sidefigure {
		page-break-inside: avoid;
		text-align: center;
		margin: 2.5em 0;
	}
	.figure img,    .sidefigure img,    figure img,
	.figure object, .sidefigure object, figure object {
		max-width: 100%;
		margin: auto;
		height: auto;
	}
	.figure pre, .sidefigure pre, figure pre {
		text-align: left;
		display: table;
		margin: 1em auto;
	}
	.figure table, figure table {
		margin: auto;
	}
	@media screen and (min-width: 20em) {
		.sidefigure {
			float: right;
			width: 50%;
			margin: 0 0 0.5em 0.5em;
		}
	}
	.caption, figcaption, caption {
		font-style: italic;
		font-size: 90%;
	}
	.caption::before, figcaption::before, figcaption > .marker {
		font-weight: bold;
	}
	.caption, figcaption {
		counter-increment: figure;
	}

	/* DL list is indented 2em, but figure inside it is not */
	dd > .figure, dd > figure { margin-left: -2em; }

/******************************************************************************/
/*                             Colored Boxes                                  */
/******************************************************************************/

	.issue, .note, .example, .advisement, blockquote {
		padding: .5em;
		border: .5em;
		border-left-style: solid;
		page-break-inside: avoid;
	}
	span.issue, span.note {
		padding: .1em .5em .15em;
		border-right-style: solid;
	}

	.issue,
	.note,
	.example,
	.advisement,
	blockquote {
		margin: 1em auto;
	}
	.note  > p:first-child,
	.issue > p:first-child,
	blockquote > :first-child {
		margin-top: 0;
	}
	blockquote > :last-child {
		margin-bottom: 0;
	}

/** Blockquotes ***************************************************************/

	blockquote {
		border-color: silver;
	}

/** Open issue ****************************************************************/

	.issue {
		border-color: #E05252;
		background: #FBE9E9;
		counter-increment: issue;
		overflow: auto;
	}
	.issue::before, .issue > .marker {
		color: #AE1E1E;
		padding-right: 1em;
		text-transform: uppercase;
	}
	/* Add .issue::before { content: \"Issue \" counter(issue) \" \"; } for autogen numbers,
	   or use class=\"marker\" to mark up the issue number in source. */

/** Example *******************************************************************/

	.example {
		border-color: #E0CB52;
		background: #FCFAEE;
		counter-increment: example;
		overflow: auto;
		clear: both;
	}
	.example::before, .example > .marker {
		text-transform: uppercase;
		color: #827017;
		min-width: 7.5em;
		display: block;
	}
	/* Add .example::before { content: \"Example \" counter(example) \" \"; } for autogen numbers,
	   or use class=\"marker\" to mark up the example number in source. */

/** Non-normative Note ********************************************************/

	.note {
		border-color: #52E052;
		background: #E9FBE9;
		overflow: auto;
	}

	.note::before, .note > .marker,
	details.note > summary::before,
	details.note > summary > .marker {
		text-transform: uppercase;
		display: block;
		color: hsl(120, 70%, 30%);
	}
	/* Add .note::before { content: \"Note \"; } for autogen label,
	   or use class=\"marker\" to mark up the label in source. */

	details.note > summary {
		color: hsl(120, 70%, 30%);
	}
	details.note[open] > summary {
		border-bottom: 1px silver solid;
	}

/** Advisement Box ************************************************************/
	/*  for attention-grabbing normative statements */

	.advisement {
		border-color: orange;
		border-style: none solid;
		background: #FFEECC;
	}
	strong.advisement {
		display: block;
		text-align: center;
	}
	.advisement > .marker {
		color: #B35F00;
	}

/** Spec Obsoletion Notice ****************************************************/
	/* obnoxious obsoletion notice for older/abandoned specs. */

	details {
		display: block;
	}
	summary {
		font-weight: bolder;
	}

	.annoying-warning:not(details),
	details.annoying-warning:not([open]) > summary,
	details.annoying-warning[open] {
		background: hsla(40,100%,50%,0.95);
		color: black;
		padding: .75em 1em;
		border: red;
		border-style: solid none;
		box-shadow: 0 2px 8px black;
		text-align: center;
	}
	.annoying-warning :last-child {
		margin-bottom: 0;
	}

@media not print {
	details.annoying-warning[open] {
		position: fixed;
		left: 0;
		right: 0;
		bottom: 2em;
		z-index: 1000;
	}
}

	details.annoying-warning:not([open]) > summary {
		text-align: center;
	}

/** Entity Definition Boxes ***************************************************/

	.def {
		padding: .5em 1em;
		background: #DEF;
		margin: 1.2em 0;
		border-left: 0.5em solid #8CCBF2;
	}

/******************************************************************************/
/*                                    Tables                                  */
/******************************************************************************/

	th, td {
		text-align: left;
		text-align: start;
	}

/** Property/Descriptor Definition Tables *************************************/

	table.def {
		/* inherits .def box styling, see above */
		width: 100%;
		border-spacing: 0;
	}

	table.def td,
	table.def th {
		padding: 0.5em;
		vertical-align: baseline;
		border-bottom: 1px solid #bbd7e9;
	}

	table.def > tbody > tr:last-child th,
	table.def > tbody > tr:last-child td {
		border-bottom: 0;
	}

	table.def th {
		font-style: italic;
		font-weight: normal;
		padding-left: 1em;
		width: 3em;
	}

	/* For when values are extra-complex and need formatting for readability */
	table td.pre {
		white-space: pre-wrap;
	}

	/* A footnote at the bottom of a def table */
	table.def           td.footnote {
		padding-top: 0.6em;
	}
	table.def           td.footnote::before {
		content: \" \";
		display: block;
		height: 0.6em;
		width: 4em;
		border-top: thin solid;
	}

/** Data tables (and properly marked-up index tables) *************************/
	/*
		 <table class=\"data\"> highlights structural relationships in a table
		 when correct markup is used (e.g. thead/tbody, th vs. td, scope attribute)

		 Use class=\"complex data\" for particularly complicated tables --
		 (This will draw more lines: busier, but clearer.)

		 Use class=\"long\" on table cells with paragraph-like contents
		 (This will adjust text alignment accordingly.)
		 Alternately use class=\"longlastcol\" on tables, to have the last column assume \"long\".
	*/

	table {
		word-wrap: normal;
		overflow-wrap: normal;
		hyphens: manual;
	}

	table.data,
	table.index {
		margin: 1em auto;
		border-collapse: collapse;
		border: hidden;
		width: 100%;
	}
	table.data caption,
	table.index caption {
		max-width: 50em;
		margin: 0 auto 1em;
	}

	table.data td,  table.data th,
	table.index td, table.index th {
		padding: 0.5em 1em;
		border-width: 1px;
		border-color: silver;
		border-top-style: solid;
	}

	table.data thead td:empty {
		padding: 0;
		border: 0;
	}

	table.data  thead,
	table.index thead,
	table.data  tbody,
	table.index tbody {
		border-bottom: 2px solid;
	}

	table.data colgroup,
	table.index colgroup {
		border-left: 2px solid;
	}

	table.data  tbody th:first-child,
	table.index tbody th:first-child  {
		border-right: 2px solid;
		border-top: 1px solid silver;
		padding-right: 1em;
	}

	table.data th[colspan],
	table.data td[colspan] {
		text-align: center;
	}

	table.complex.data th,
	table.complex.data td {
		border: 1px solid silver;
		text-align: center;
	}

	table.data.longlastcol td:last-child,
	table.data td.long {
	 vertical-align: baseline;
	 text-align: left;
	}

	table.data img {
		vertical-align: middle;
	}


/*
Alternate table alignment rules

	table.data,
	table.index {
		text-align: center;
	}

	table.data  thead th[scope=\"row\"],
	table.index thead th[scope=\"row\"] {
		text-align: right;
	}

	table.data  tbody th:first-child,
	table.index tbody th:first-child  {
		text-align: right;
	}

Possible extra rowspan handling

	table.data  tbody th[rowspan]:not([rowspan='1']),
	table.index tbody th[rowspan]:not([rowspan='1']),
	table.data  tbody td[rowspan]:not([rowspan='1']),
	table.index tbody td[rowspan]:not([rowspan='1']) {
		border-left: 1px solid silver;
	}

	table.data  tbody th[rowspan]:first-child,
	table.index tbody th[rowspan]:first-child,
	table.data  tbody td[rowspan]:first-child,
	table.index tbody td[rowspan]:first-child{
		border-left: 0;
		border-right: 1px solid silver;
	}
*/

/******************************************************************************/
/*                                  Indices                                   */
/******************************************************************************/


/** Table of Contents *********************************************************/

	.toc a {
		/* More spacing; use padding to make it part of the click target. */
		padding-top: 0.1rem;
		/* Larger, more consistently-sized click target */
		display: block;
		/* Reverse color scheme */
		color: black;
		border-color: #3980B5;
	}
	.toc a:visited {
		border-color: #054572;
	}
	.toc a:not(:focus):not(:hover) {
		/* Allow colors to cascade through from link styling */
		border-bottom-color: transparent;
	}

	.toc, .toc ol, .toc ul, .toc li {
		list-style: none; /* Numbers must be inlined into source */
		/* because generated content isn't search/selectable and markers can't do multilevel yet */
		margin:  0;
		padding: 0;
	}
	.toc {
		line-height: 1.1em; /* consistent spacing */
	}

	/* ToC not indented until third level, but font style & margins show hierarchy */
	.toc > li             { font-weight: bold;   }
	.toc > li li          { font-weight: normal; }
	.toc > li li li       { font-size:   95%;    }
	.toc > li li li li    { font-size:   90%;    }
	.toc > li li li li li { font-size:   85%;    }

	.toc > li             { margin: 1.5rem 0;    }
	.toc > li li          { margin: 0.3rem 0;    }
	.toc > li li li       { margin-left: 2rem;   }

	/* Section numbers in a column of their own */
	.toc .secno {
		float: left;
		width: 4rem;
		white-space: nowrap;
	}
	.toc > li li li li .secno {
		font-size: 85%;
	}
	.toc > li li li li li .secno {
		font-size: 100%;
	}

	:not(li) > .toc              { margin-left:  5rem; }
	.toc .secno                  { margin-left: -5rem; }
	.toc > li li li .secno       { margin-left: -7rem; }
	.toc > li li li li .secno    { margin-left: -9rem; }
	.toc > li li li li li .secno { margin-left: -11rem; }

	/* Tighten up indentation in narrow ToCs */
	@media (max-width: 30em) {
		:not(li) > .toc              { margin-left:  4rem; }
		.toc .secno                  { margin-left: -4rem; }
		.toc > li li li              { margin-left:  1rem; }
		.toc > li li li .secno       { margin-left: -5rem; }
		.toc > li li li li .secno    { margin-left: -6rem; }
		.toc > li li li li li .secno { margin-left: -7rem; }
	}
	@media screen and (min-width: 78em) {
		body:not(.toc-inline) :not(li) > .toc              { margin-left:  4rem; }
		body:not(.toc-inline) .toc .secno                  { margin-left: -4rem; }
		body:not(.toc-inline) .toc > li li li              { margin-left:  1rem; }
		body:not(.toc-inline) .toc > li li li .secno       { margin-left: -5rem; }
		body:not(.toc-inline) .toc > li li li li .secno    { margin-left: -6rem; }
		body:not(.toc-inline) .toc > li li li li li .secno { margin-left: -7rem; }
	}
	body.toc-sidebar #toc :not(li) > .toc              { margin-left:  4rem; }
	body.toc-sidebar #toc .toc .secno                  { margin-left: -4rem; }
	body.toc-sidebar #toc .toc > li li li              { margin-left:  1rem; }
	body.toc-sidebar #toc .toc > li li li .secno       { margin-left: -5rem; }
	body.toc-sidebar #toc .toc > li li li li .secno    { margin-left: -6rem; }
	body.toc-sidebar #toc .toc > li li li li li .secno { margin-left: -7rem; }

	.toc li {
		clear: both;
	}


/** Index *********************************************************************/

	/* Index Lists: Layout */
	ul.index       { margin-left: 0; columns: 15em; text-indent: 1em hanging; }
	ul.index li    { margin-left: 0; list-style: none; break-inside: avoid; }
	ul.index li li { margin-left: 1em; }
	ul.index dl    { margin-top: 0; }
	ul.index dt    { margin: .2em 0 .2em 20px;}
	ul.index dd    { margin: .2em 0 .2em 40px;}
	/* Index Lists: Typography */
	ul.index ul,
	ul.index dl { font-size: smaller; }
	@media not print {
		ul.index li span {
			white-space: nowrap;
			color: transparent;
		}
		ul.index li a:hover + span,
		ul.index li a:focus + span {
			color: #707070;
		}
	}

/** Index Tables *****************************************************/
	/* See also the data table styling section, which this effectively subclasses */

	table.index {
		font-size: small;
		border-collapse: collapse;
		border-spacing: 0;
		text-align: left;
		margin: 1em 0;
	}

	table.index td,
	table.index th {
		padding: 0.4em;
	}

	table.index tr:hover td:not([rowspan]),
	table.index tr:hover th:not([rowspan]) {
		background: #f7f8f9;
	}

	/* The link in the first column in the property table (formerly a TD) */
	table.index th:first-child a {
		font-weight: bold;
	}

/** Outdated warning **********************************************************/

a#outdated-note {
	color: white;
}

a#outdated-note:hover {
	background: transparent;
}

.outdated-spec {
	background-color: rgba(0,0,0,0.5);
}

.outdated-warning {
	position: fixed;
	bottom: 50%;
	left: 0;
	right: 0;
	margin: 0 auto;
	width: 50%;
	background: maroon;
	color: white;
	border-radius: 1em;
	box-shadow: 0 0 1em red;
	padding: 2em;
	text-align: center;
	z-index: 2;
}

.edited-rec-warning {
	background: darkorange;
	box-shadow: 0 0 1em;
}

.outdated-warning button {
	position: absolute;
	top: 0;
	right:0;
	margin: 0;
	border: 0;
	padding: 0.25em 0.5em;
	background: transparent;
	color: white;
	font:1em sans-serif;
	text-align:center;
}

.outdated-warning span {
	display: block;
}

.outdated-collapsed {
	bottom: 0;
	border-radius: 0;
	width: 100%;
	padding: 0;
}

/******************************************************************************/
/*                                    Print                                   */
/******************************************************************************/

	@media print {
		/* Pages have their own margins. */
		html {
			margin: 0;
		}
		/* Serif for print. */
		body {
			font-family: serif;
		}

		.outdated-warning {
			position: absolute;
			border-style: solid;
			border-color: red;
		}

		.outdated-warning input {
			display: none;
		}
	}
	@page {
		margin: 1.5cm 1.1cm;
	}

/******************************************************************************/
/*                                    Legacy                                  */
/******************************************************************************/

	/* This rule is inherited from past style sheets. No idea what it's for. */
	.hide { display: none; }



/******************************************************************************/
/*                             Overflow Control                               */
/******************************************************************************/

	.figure .caption, .sidefigure .caption, figcaption {
		/* in case figure is overlarge, limit caption to 50em */
		max-width: 50rem;
		margin-left: auto;
		margin-right: auto;
	}
	.overlarge {
		/* Magic to create good item positioning:
		   \"content column\" is 50ems wide at max; less on smaller screens.
		   Extra space (after ToC + content) is empty on the right.

		   1. When item < content column, centers item in column.
		   2. When content < item < available, left-aligns.
		   3. When item > available, fills available and is scrollable.
		*/
		display: grid;
		grid-template-columns: minmax(0, 50em);
	}
	.overlarge > table {
		/* limit preferred width of table */
		max-width: 50em;
		margin-left: auto;
		margin-right: auto;
	}

	@media (min-width: 55em) {
		.overlarge {
			margin-right: calc(13px + 26.5rem - 50vw);
			max-width: none;
		}
	}
	@media screen and (min-width: 78em) {
		body:not(.toc-inline) .overlarge {
			/* 30.5em body padding 50em content area */
			margin-right: calc(40em - 50vw) !important;
		}
	}
	@media screen and (min-width: 90em) {
		body:not(.toc-inline) .overlarge {
			/* 4em html margin 30.5em body padding 50em content area */
			margin-right: calc(84.5em - 100vw) !important;
		}
	}

	@media not print {
		.overlarge {
			overflow-x: auto;
			/* See Lea Verou's explanation background-attachment:
			 * http://lea.verou.me/2012/04/background-attachment-local/
			 *
			background: top left  / 4em 100% linear-gradient(to right,  #ffffff, rgba(255, 255, 255, 0)) local,
			            top right / 4em 100% linear-gradient(to left, #ffffff, rgba(255, 255, 255, 0)) local,
			            top left  / 1em 100% linear-gradient(to right,  #c3c3c5, rgba(195, 195, 197, 0)) scroll,
			            top right / 1em 100% linear-gradient(to left, #c3c3c5, rgba(195, 195, 197, 0)) scroll,
			            white;
			background-repeat: no-repeat;
			*/
		}
	}

")
