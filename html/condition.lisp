(in-package :html)

(define-condition html-condition () ())

(define-condition html-error (html-condition error) ())

(define-condition parse-error (html-error) ())

(defmacro define-parse-error (name &key description))

(define-parse-error abrupt-closing-of-empty-comment
  :description "This error occurs if the parser encounters an empty comment that is abruptly closed by a U+003E (>) code point (i.e., <!--> or <!--->). The parser behaves as if the comment is closed correctly.")

(define-parse-error abrupt-doctype-public-identifier
  :description "This error occurs if the parser encounters a U+003E (>) code point in the DOCTYPE public identifier (e.g., <!DOCTYPE html PUBLIC \"foo>). In such a case, if the DOCTYPE is correctly placed as a document preamble, the parser sets the Document to quirks mode.")

(define-parse-error abrupt-doctype-system-identifier
  :description "This error occurs if the parser encounters a U+003E (>) code point in the DOCTYPE system identifier (e.g., <!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"foo>). In such a case, if the DOCTYPE is correctly placed as a document preamble, the parser sets the Document to quirks mode.")

(define-parse-error absence-of-digits-in-numeric-character-reference
  :description "This error occurs if the parser encounters a numeric character reference that doesn't contain any digits (e.g., &#qux;). In this case the parser doesn't resolve the character reference.")

(define-parse-error cdata-in-html-content
  :description "This error occurs if the parser encounters a CDATA section outside of foreign content (SVG or MathML). The parser treats such CDATA sections (including leading \"[CDATA[\" and trailing \"]]\" strings) as comments.")

(define-parse-error character-reference-outside-unicode-range
  :description "This error occurs if the parser encounters a numeric character reference that references a code point that is greater than the valid Unicode range. The parser resolves such a character reference to a U+FFFD REPLACEMENT CHARACTER.")

(define-parse-error control-character-in-input-stream
  :description "This error occurs if the input stream contains a control code point that is not ASCII whitespace or U+0000 NULL. Such code points are parsed as-is and usually, where parsing rules don't apply any additional restrictions, make their way into the DOM.")

(define-parse-error control-character-reference
  :description "This error occurs if the parser encounters a numeric character reference that references a control code point that is not ASCII whitespace or is a U+000D CARRIAGE RETURN. The parser resolves such character references as-is except C1 control references that are replaced according to the numeric character reference end state.")

(define-parse-error end-tag-with-attributes
  :description "This error occurs if the parser encounters an end tag with attributes. Attributes in end tags are completely ignored and do not make their way into the DOM.")

(define-parse-error duplicate-attribute
  :description "This error occurs if the parser encounters an attribute in a tag that already has an attribute with the same name. The parser ignores all such duplicate occurrences of the attribute.")

(define-parse-error end-tag-with-trailing-solidus
  :description "This error occurs if the parser encounters an end tag that has a U+002F (/) code point right before the closing U+003E (>) code point (e.g., </div/>). Such a tag is treated as a regular end tag.")

(define-parse-error eof-before-tag-name
  :description "This error occurs if the parser encounters the end of the input stream where a tag name is expected. In this case the parser treats the beginning of a start tag (i.e., <) or an end tag (i.e., </) as text content.")

(define-parse-error eof-in-cdata
  :description "This error occurs if the parser encounters the end of the input stream in a CDATA section. The parser treats such CDATA sections as if they are closed immediately before the end of the input stream.")

(define-parse-error eof-in-comment
  :description "This error occurs if the parser encounters the end of the input stream in a comment. The parser treats such comments as if they are closed immediately before the end of the input stream.")

(define-parse-error eof-in-doctype
  :description "This error occurs if the parser encounters the end of the input stream in a DOCTYPE. In such a case, if the DOCTYPE is correctly placed as a document preamble, the parser sets the Document to quirks mode.")

(define-parse-error eof-in-script-html-comment-like-text
  :description "This error occurs if the parser encounters the end of the input stream in text that resembles an HTML comment inside script element content (e.g., <script><!-- foo).")

(define-parse-error eof-in-tag
  :description "This error occurs if the parser encounters the end of the input stream in a start tag or an end tag (e.g., <div id=). Such a tag is completely ignored.")

(define-parse-error incorrectly-closed-comment
  :description "This error occurs if the parser encounters a comment that is closed by the \"--!>\" code point sequence. The parser treats such comments as if they are correctly closed by the \"-->\" code point sequence.")

(define-parse-error incorrectly-opened-comment
  :description "This error occurs if the parser encounters the \"<!\" code point sequence that is not immidiately followed by two U+002D (-) code points and that is not the start of a DOCTYPE or a CDATA section. All content that follows the \"<!\" code point sequence up to a U+003E (>) code point (if present) or to the end of the input stream is treated as a comment.")

(define-parse-error invalid-character-sequence-after-doctype-name
  :description "This error occurs if the parser encounters any code point sequence other than \"PUBLIC\" and \"SYSTEM\" keywords after a DOCTYPE name. In such a case, the parser ignores any following public or system identifiers, and if the DOCTYPE is correctly placed as a document preamble, sets the Document to quirks mode.")

(define-parse-error invalid-first-character-of-tag-name
  :description "This error occurs if the parser encounters a code point that is not an ASCII alpha where first code point of a start tag name or an end tag name is expected. If a start tag was expected such code point and a preceding U+003C (<) is treated as text content, and all content that follows is treated as markup. Whereas, if an end tag was expected, such code point and all content that follows up to a U+003E (>) code point (if present) or to the end of the input stream is treated as a comment.")

(define-parse-error missing-attribute-value
  :description "This error occurs if the parser encounters a U+003E (>) code point where an attribute value is expected (e.g., <div id=>). The parser treats the attribute as having an empty value.")

(define-parse-error invalid-first-character-of-tag-name
  :description "This error occurs if the parser encounters a DOCTYPE that is missing a name (e.g., <!DOCTYPE>). In such a case, if the DOCTYPE is correctly placed as a document preamble, the parser sets the Document to quirks mode.")

(define-parse-error missing-doctype-public-identifier
  :description "This error occurs if the parser encounters a U+003E (>) code point where start of the DOCTYPE public identifier is expected (e.g., <!DOCTYPE html PUBLIC >). In such a case, if the DOCTYPE is correctly placed as a document preamble, the parser sets the Document to quirks mode.")

(define-parse-error missing-doctype-system-identifier
  :description "This error occurs if the parser encounters a U+003E (>) code point where start of the DOCTYPE system identifier is expected (e.g., <!DOCTYPE html SYSTEM >). In such a case, if the DOCTYPE is correctly placed as a document preamble, the parser sets the Document to quirks mode.")

(define-parse-error missing-end-tag-name
  :description "This error occurs if the parser encounters a U+003E (>) code point where an end tag name is expected, i.e., </>. The parser completely ignores whole \"</>\" code point sequence.")

(define-parse-error missing-quote-before-doctype-public-identifier
  :description "This error occurs if the parser encounters the DOCTYPE public identifier that is not preceded by a quote (e.g., <!DOCTYPE html PUBLIC -//W3C//DTD HTML 4.01//EN\">). In such a case, the parser ignores the public identifier, and if the DOCTYPE is correctly placed as a document preamble, sets the Document to quirks mode.")

(define-parse-error missing-quote-before-doctype-system-identifier
  :description "This error occurs if the parser encounters the DOCTYPE system identifier that is not preceded by a quote (e.g., <!DOCTYPE html SYSTEM http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">). In such a case, the parser ignores the system identifier, and if the DOCTYPE is correctly placed as a document preamble, sets the Document to quirks mode.")

(define-parse-error missing-semicolon-after-character-reference
  :description "This error occurs if the parser encounters a character reference that is not terminated by a U+003B (;) code point. Usually the parser behaves as if character reference is terminated by the U+003B (;) code point; however, there are some ambiguous cases in which the parser includes subsequent code points in the character reference.")

(define-parse-error missing-whitespace-after-doctype-public-keyword
  :description "This error occurs if the parser encounters a DOCTYPE whose \"PUBLIC\" keyword and public identifier are not separated by ASCII whitespace. In this case the parser behaves as if ASCII whitespace is present.")

(define-parse-error missing-whitespace-after-doctype-system-keyword
  :description "This error occurs if the parser encounters a DOCTYPE whose \"SYSTEM\" keyword and system identifier are not separated by ASCII whitespace. In this case the parser behaves as if ASCII whitespace is present.")

(define-parse-error missing-whitespace-before-doctype-name
  :description "This error occurs if the parser encounters a DOCTYPE whose \"DOCTYPE\" keyword and name are not separated by ASCII whitespace. In this case the parser behaves as if ASCII whitespace is present.")

(define-parse-error missing-whitespace-between-attributes
  :description "This error occurs if the parser encounters attributes that are not separated by ASCII whitespace (e.g., <div id=\"foo\"class=\"bar\">). In this case the parser behaves as if ASCII whitespace is present.")

(define-parse-error missing-whitespace-between-doctype-public-and-system-identifiers
  :description "This error occurs if the parser encounters a DOCTYPE whose public and system identifiers are not separated by ASCII whitespace. In this case the parser behaves as if ASCII whitespace is present.")

(define-parse-error nested-comment
  :description "This error occurs if the parser encounters a nested comment (e.g., <!-- <!-- nested --> -->). Such a comment will be closed by the first occuring \"-->\" code point sequence and everything that follows will be treated as markup.")

(define-parse-error noncharacter-character-reference
  :description "This error occurs if the parser encounters a numeric character reference that references a noncharacter. The parser resolves such character references as-is.")

(define-parse-error noncharacter-in-input-stream
  :description "This error occurs if the input stream contains a noncharacter. Such code points are parsed as-is and usually, where parsing rules don't apply any additional restrictions, make their way into the DOM.")

(define-parse-error non-void-html-element-start-tag-with-trailing-solidus
  :description "This error occurs if the parser encounters a start tag for an element that is not in the list of void elements or is not a part of foreign content (i.e., not an SVG or MathML element) that has a U+002F (/) code point right before the closing U+003E (>) code point. The parser behaves as if the U+002F (/) is not present.")

(define-parse-error null-character-reference
  :description "This error occurs if the parser encounters a numeric character reference that references a U+0000 NULL code point. The parser resolves such character references to a U+FFFD REPLACEMENT CHARACTER.")

(define-parse-error surrogate-character-reference
  :description "This error occurs if the parser encounters a numeric character reference that references a surrogate. The parser resolves such character references to a U+FFFD REPLACEMENT CHARACTER.")

(define-parse-error surrogate-in-input-stream
  :description "This error occurs if the input stream contains a surrogate. Such code points are parsed as-is and usually, where parsing rules don't apply any additional restrictions, make their way into the DOM.")

(define-parse-error unexpected-character-after-doctype-system-identifier
  :description "This error occurs if the parser encounters any code points other than ASCII whitespace or closing U+003E (>) after the DOCTYPE system identifier. The parser ignores these code points.")

(define-parse-error unexpected-character-in-attribute-name
  :description "This error occurs if the parser encounters a U+0022 (\"), U+0027 ('), or U+003C (<) code point in an attribute name. The parser includes such code points in the attribute name.")

(define-parse-error unexpected-character-in-unquoted-attribute-value
  :description "This error occurs if the parser encounters a U+0022 (\"), U+0027 ('), U+003C (<), U+003D (=), or U+0060 (`) code point in an unquoted attribute value. The parser includes such code points in the attribute value.")

(define-parse-error unexpected-equals-sign-before-attribute-name
  :description "This error occurs if the parser encounters a U+003D (=) code point before an attribute name. In this case the parser treats U+003D (=) as the first code point of the attribute name.")

(define-parse-error unexpected-null-character
  :description "This error occurs if the parser encounters a U+0000 NULL code point in the input stream in certain positions. In general, such code points are either completely ignored or, for security reasons, replaced with a U+FFFD REPLACEMENT CHARACTER.")

(define-parse-error unexpected-question-mark-instead-of-tag-name
  :description "This error occurs if the parser encounters a U+003F (?) code point where first code point of a start tag name is expected. The U+003F (?) and all content that follows up to a U+003E (>) code point (if present) or to the end of the input stream is treated as a comment.")

(define-parse-error unexpected-solidus-in-tag
  :description "This error occurs if the parser encounters a U+002F (/) code point that is not a part of a quoted attribute value and not immediately followed by a U+003E (>) code point in a tag (e.g., <div / id=\"foo\">). In this case the parser behaves as if it encountered ASCII whitespace.")

(define-parse-error unknown-named-character-reference
  :description "This error occurs if the parser encounters an ambiguous ampersand. In this case the parser doesn't resolve the character reference.")