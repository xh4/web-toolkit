;;; package.lisp

(in-package :cl-user)
(defpackage #:cl-change-case-test
  (:use :cl :fiveam :cl-change-case))

(in-package :cl-change-case-test)

(def-suite :cl-change-case)
(in-suite :cl-change-case)

(defmacro is-string (actual expected)
  `(is (string= ,actual ,expected)))


;;; lower case tests

(test lower-case-strings
  (is-string (lower-case nil) "")
  (is-string (lower-case "TEST") "test")
  (is-string (lower-case "test") "test"))

(test lower-case-first
  (is-string (lower-case-first nil) "")
  (is-string (lower-case-first "Test") "test")
  (is-string (lower-case-first "TEST") "tEST"))

(test string-lower-case-p
  (is (not (null (string-lower-case-p "test"))))
  (is (not (null (string-lower-case-p "test string!"))))
  (is (null (string-lower-case-p "Test")))
  (is (null (string-lower-case-p "TEST"))))


;;; upper case tests

(test upper-case
  (is-string (upper-case nil) "")
  (is-string (upper-case "test") "TEST")
  (is-string (upper-case "TEST") "TEST")
  (is-string (upper-case "string") "STRING"))

(test upper-case-first
  (is-string (upper-case-first nil) "")
  (is-string (upper-case-first "test") "Test")
  (is-string (upper-case-first "TEST") "TEST"))

(test string-upper-case-p
  (is (not (null (string-upper-case-p "TEST"))))
  (is (not (null (string-upper-case-p "TEST STRING!"))))
  (is (null (string-upper-case-p "test")))
  (is (null (string-upper-case-p "Test"))))


;;; no case tests

(defparameter *no-case-tests*
  '(
    ;; single words
    ("test" "test")
    ("TEST" "test")

    ;; camel case
    ("testString" "test string")
    ("testString123" "test string123")
    ("testString_1_2_3" "test string 1 2 3")
    ("x_256" "x 256")
    ("anHTMLTag" "an html tag")
    ("ID123String" "id123 string")
    ("Id123String" "id123 string")
    ("foo bar123" "foo bar123")
    ("a1bStar" "a1b star")

    ;; Constant case
    ("CONSTANT_CASE" "constant case")
    ("CONST123_FOO" "const123 foo")

    ;; Random case
    ("FOO_bar" "foo bar")

    ;; non-alphanumeric separators
    ("dot.case" "dot case")
    ("path/case" "path case")
    ("snake_case" "snake case")
    ("snake_case123" "snake case123")
    ("snake_case_123" "snake case 123")

    ;; Punctuation
    ("\"quotes\"" "quotes")

    ;; Space between number parts
    ("version 0.45.0" "version 0 45 0")
    ("version 0..78..9" "version 0 78 9")
    ("version 4_99/4" "version 4 99 4")

    ;; Odd input
    (nil "")
    (10 "10")

    ;; Whitespace
    ("   test   " "test")

    ;; number string
    ("something_2014_other" "something 2014 other")
    ("amazon s3 data" "amazon s3 data")
    ("foo_13_bar" "foo 13 bar")))

(test no-case
  (loop for (input expected) in *no-case-tests*
        do (is-string (no-case input) expected))
  ;; test with custom replacement character
  (is-string (no-case "HELLO WORLD!" :replacement "_") "hello_world"))


;;; camel case tests

(test camel-case
  ;; lowercase single words
  (is-string (camel-case "test") "test")
  (is-string (camel-case "TEST") "test")

  ;; camelCase regular no cased strings
  (is-string (camel-case "test string") "testString")
  (is-string (camel-case "Test String") "testString")

  ;; camel case non-alphanumeric chars
  (is-string (camel-case "dot.case") "dotCase")
  (is-string (camel-case "path/case") "pathCase")

  ;; underscore dots between numbers
  (is-string (camel-case "version 1.2.10") "version_1_2_10")
  (is-string (camel-case "version 1.21.0") "version_1_21_0")

  ;; camel case pascal cased stringd
  (is-string (camel-case "TestString") "testString")

  ;; smash numbers if wished
  (is-string (camel-case "test 1 2 3" :merge-numbers t) "test123"))


;;; dot case tests

(test dot-case
  ;; single words
  (is-string (dot-case "test") "test")
  (is-string (dot-case "TEST") "test")

  ;; no cased strings
  (is-string (dot-case "test string") "test.string")
  (is-string (dot-case "Test String") "test.string")

  ;; strings with non-alphanumeric chars
  (is-string (dot-case "dot.case") "dot.case")
  (is-string (dot-case "path/case") "path.case")

  ;; various
  (is-string (dot-case "TestString") "test.string")
  (is-string (dot-case "TestString1_2_3") "test.string1.2.3")
  (is-string (dot-case "TestString_1_2_3") "test.string.1.2.3"))


;;; header case tests

(test header-case
  ;; single words
  (is-string (header-case "test") "Test")
  (is-string (header-case "TEST") "Test")

  ;; no cased strings
  (is-string (header-case "test string") "Test-String")
  (is-string (header-case "Test String") "Test-String")

  ;; strings with non-alphanumeric chars
  (is-string (header-case "dot.case") "Dot-Case")
  (is-string (header-case "path/case") "Path-Case")

  ;; various
  (is-string (header-case "TestString") "Test-String")
  (is-string (header-case "TestString1_2_3") "Test-String1-2-3")
  (is-string (header-case "TestString_1_2_3") "Test-String-1-2-3"))


;;; swap case tests

(test swap-case
  (is-string (swap-case "test") "TEST")
  (is-string (swap-case "TEST") "test")
  (is-string (swap-case "PascalCase") "pASCALcASE"))


;;; title case tests

(test title-case
  ;; single words
  (is-string (title-case "test") "Test")
  (is-string (title-case "TEST") "Test")

  ;; no cased strings
  (is-string (title-case "test string") "Test String")
  (is-string (title-case "Test String") "Test String")

  ;; strings with non-alphanumeric chars
  (is-string (title-case "dot.case") "Dot Case")
  (is-string (title-case "path/case") "Path Case")

  ;; various
  (is-string (title-case "TestString") "Test String")
  (is-string (title-case "TestString1_2_3") "Test String1 2 3")
  (is-string (title-case "TestString_1_2_3") "Test String 1 2 3"))


;;; other tests

(test others
  (is-string (param-case "test string") "test-string")
  (is-string (pascal-case "test string") "TestString")
  (is-string (path-case "test string") "test/string")
  (is-string (sentence-case "test string") "Test string")
  (is-string (snake-case "test string") "test_string")
  (is-string (constant-case "test string") "TEST_STRING"))
