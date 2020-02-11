[![Build Status](https://travis-ci.org/rudolfochrist/cl-change-case.svg?branch=master)](https://travis-ci.org/rudolfochrist/cl-change-case) [![Coverage Status](https://coveralls.io/repos/github/rudolfochrist/cl-change-case/badge.svg?branch=master)](https://coveralls.io/github/rudolfochrist/cl-change-case?branch=master) [![Quicklisp](http://quickdocs.org/badge/cl-change-case.svg)](http://quickdocs.org/cl-change-case/)


# NAME

cl-change-case &#x2014; Convert strings between camelCase, param-case, PascalCase and more


# VERSION

    Version 0.1.0


# SYNOPSIS

    (use-package :cl-change-case)
    
    (format t "~{~S~%~}"
            (list (camel-case "test string")
                  (param-case "test string")
                  (pascal-case "test string")))


# DESCRIPTION

`cl-change-case` is library to convert strings between `camelCase`, `PascalCase`, `snake_case`, `param-case`,
`CONSTANT_CASE` and more. 

This is a Common Lisp port of [blakeembrey/change-case](https://github.com/blakeembrey/change-case) released under [The MIT License](https://opensource.org/licenses/MIT). 


## Functions


### lower-case

Return the string in lower case.

    (lower-case "TEST STRING")

    "test string"


### lower-case-first

Lower case of the first character of string.

    (lower-case-first "TEST STRING")

    "tEST STRING"


### string-lower-case-p

Test if all characters in string have lower case.

    (string-lower-case-p "test string")

    T


### upper-case

Return the string in upper case.

    (upper-case "test string")

    "TEST STRING"


### upper-case-first

Upper case the first character of string.

    (upper-case-first "test string")

    "Test string"


### string-upper-case-p

Test if all characters in string have upper case.

    (string-upper-case-p "TEST STRING")

    T


### no-case

Make string a lower case, space separated string. 

    (no-case "test_stringTest")

    "test string test"

Optionally you can provide a different replacement string.

    (no-case "test_stringTest" :replacement "$$")

    "test$$string$$test"


### camel-case

Convert string to `camelCase`.

    (camel-case "test_string")

    "testString"


### dot-case

Convert string to `dot.case`.

    (dot-case "Test String")

    "test.string"


### header-case

Title case string but dash separated.

    (header-case "test string")

    "Test-String"


### param-case

Convert string to `param-case`.

    (param-case "test string")

    "test-string"


### pascal-case

Convert string to `PascalCase`.

    (pascal-case "test string")

    "TestString"


### path-case

Convert string to `path/case`.

    (path-case "test string more")

    "test/string/more"


### sentence-case

Makes string a lower case, space separated string with the first word capitalized.

    (sentence-case "thisIsATestString")

    "This is a test string"


### snake-case

Convert string to `snake_case`.

    (snake-case "test string")

    "test_string"


### swap-case

Reverse the case of each character in string.

    (swap-case "PascalCase")

    "pASCALcASE"


### title-case

Make string space separated with each word capitalized.

    (title-case "this_is a_test_string")

    "This Is A Test String"


### constant-case

Convert string to `CONSTANT_CASE`.

    (constant-case "test string")

    "TEST_STRING"


# AUTHOR

Sebastian Christ (<rudolfo.christ@gmail.com>)


# COPYRIGHT

Copyright (c) 2016 Sebastian Christ (rudolfo.christ@gmail.com)

Released under the LLGPL license.


# SEE ALSO

-   [blakeembrey/change-case](https://github.com/blakeembrey/change-case)

