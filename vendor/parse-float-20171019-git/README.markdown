This package exports the following function to parse floating-point values from a string in common lisp.

The information below is purposefully as close as possible to the text and format of [**parse-integer**](http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm "parse-integer in CLHS") in the [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm "CLHS"), but this package is not related to [LispWorks](http://www.lispworks.com/ "LispWorks") in any way.

#### Syntax:

**parse-float** _string_ &key _start_ _end_ _radix_ _junk-allowed_ _decimal-character_ _exponent-character_ _type_ => _float_, _pos_

#### Arguments and Values:

_string_---a [string](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#string "string in CLHS").

_start_, _end_---[bounding index designators](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#bounding_index_designator "bounding index designator in CLHS") of _string_. The defaults for start and end are 0 and [**nil**](http://www.lispworks.com/documentation/HyperSpec/Body/a_nil.htm#nil "nil in CLHS"), respectively.

_radix_---a [radix](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_r.htm#radix "radix in CLHS"). The default is 10.

_junk-allowed_---a [generalized boolean](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean "generalized boolean in CLHS"). The default is [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "false in CLHS").

_decimal-character_---a [character](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "character in CLHS") separating the integer and decimal parts. The default is `#\.`.

_exponent-character_---the exponentiation [character](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "character in CLHS") (case insensitive). The default is `#\e`.

_type_---a [number](http://www.lispworks.com/documentation/HyperSpec/Body/t_number.htm#number "number in CLHS") [type specifier](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#type_specifier "type specifier in CLHS"). The default is [**\*READ-DEFAULT-FLOAT-FORMAT\***](http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_def.htm "*READ-DEFAULT-FLOAT-FORMAT* in CLHS").

_float_---a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "float CLHS"), depending on _type_, or [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "false in CLHS").

_pos_---a [bounding index](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#bounding_index "bounding index in CLHS") of _string_.

#### Description:

**parse-float** parses a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "float in CLHS") in the specified _radix_ from the substring of _string_ delimited by _start_ and _end_ into a [number](http://www.lispworks.com/documentation/HyperSpec/Body/t_number.htm#number "number in CLHS") of the given _type_.

**parse-float** expects an optional sign (+ or -) followed by a a non-empty sequence of digits to be interpreted in the specified _radix_, optionally followed by _decimal-character_, a sequence of digits, _exponent-character_, a sign (+ or -) and a sequence of digits. Optional leading and trailing [whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace "whitespace in CLHS") is ignored.

**parse-float** does not recognize the syntactic radix-specifier prefixes `#O`, `#B`, `#X`, and `#nR`, nor does it recognize the exponent if _radix_ is not 10. 

If _junk-allowed_ is _false_, an error of [type](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#type "type in CLHS") [**parse-error**](http://www.lispworks.com/documentation/HyperSpec/Body/e_parse_.htm#parse-error "parse-error in CLHS") is signaled if substring does not consist entirely of the representation of a signed [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "float in CLHS"), possibly surrounded on either side by [whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace "whitespace in CLHS") [characters](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "character in CLHS").

The first [value](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_v.htm#value "value in CLHS") returned is either the [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "float in CLHS") that was parsed, or else [**nil**](http://www.lispworks.com/documentation/HyperSpec/Body/a_nil.htm#nil "nil in CLHS") if no syntactically correct [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "float in CLHS") was seen but _junk-allowed_ was [true](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#true "true in CLHS").

The second [value](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_v.htm#value "value in CLHS") is either the index into the [string](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#string "string in CLHS") of the delimiter that terminated the parse, or the upper [bounding index](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_b.htm#bounding_index "bounding index in CLHS") of the substring if the parse terminated at the end of the substring (as is always the case if _junk-allowed_ is [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "false in CLHS")).

#### Examples:

Load the package **parse-float** using [Quicklisp](https://www.quicklisp.org/beta/):

 `(ql:quickload "parse-float")`

Import the **parse-float** function:

 `(use-package :parse-float)`

Use the imported **parse-float** function:

 `(parse-float "123") =>  123.0, 3`

 `(parse-float "123.1" :start 1 :radix 5 :type 'double-float) =>  13.2d0, 5`
 
 `(parse-float "123,0D2" :decimal-character #\, :exponent-character #\d :type 'single-float) =>  12300.0, 7`
 
 `(parse-float "no-integer" :junk-allowed t) =>  NIL, 0`

 `(parse-float "1.2e-3" :type 'number) => 3/2500, 6`

#### Side Effects:
 
None.

#### Affected By:

None.

#### Exceptional Situations:

If _junk-allowed_ is [false](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#false "false in CLHS"), an error is signaled if substring does not consist entirely of the representation of a [float](http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm#float "float in CLHS"), possibly surrounded on either side by [whitespace](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace "whitespace in CLHS") [characters](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character "character in CLHS").

#### See Also:

[**parse-integer**](http://www.lispworks.com/documentation/HyperSpec/Body/f_parse_.htm "parse-integer in CLHS"), [**parse-number**] (https://github.com/sharplispers/parse-number "parse-number on Github")
