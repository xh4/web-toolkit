;;  cl-uri-templates
;;  Extensive URI-Templates implementation in Common-Lisp.
;;
;;  Copyright 2009 Thomas de Grivel <billitch@gmail.com>
;;  Copyright (c) 2007, 2008, 2009 Vladimir Sedach
;;
;;  This software is provided "AS IS".
;;  Please see COPYING for details.

(require 'asdf)
(require 'cl-uri-templates)
(require 'FiveAM)

(in-package #:cl-user)

(defpackage #:cl-uri-templates.test
  (:use #:common-lisp #:cl-uri-templates #:FiveAM)
  (:export #:run-tests
           #:run-interpolation-tests
           #:run-destructuring-tests))

(in-package #:cl-uri-templates.test)


(defmacro define-fixture (name args &body body)
  `(handler-case
       (def-fixture ,name ,args ,@body)
     (warning nil)))


(def-suite disabled-tests)
(def-suite enabled-tests)


(def-suite expansions :in enabled-tests)
(in-suite expansions)


(define-fixture uri-template-syntax ()
  (let ((*readtable* (copy-readtable nil)))
    (enable-uri-template-syntax)
    (&body)))


(defmacro eval-read (string)
  `(eval (read-from-string ,string)))


(test read-macro
  (with-fixture uri-template-syntax ()
    (is-true (read-from-string "#U"))
    (is (string= "" (eval-read "#U")))
    (is (string= "/" (eval-read "#U/")))
    (is (string= "" (eval-read "#U ")))
    (is (string= "a" (eval-read "#Ua ")))
    (is (string= "abc" (eval-read "#Uabc ")))
    (is (string= "a/b:c" (eval-read "#Ua/b:c")))
    (is (string= "%12" (eval-read "#U%12")))
    (is (string= "a%12/b:c" (eval-read "#Ua%12/b:c")))
    (signals (end-of-file) (eval-read "#U{"))
    (signals (end-of-file) (eval-read "#U{a"))
    (signals (end-of-file) (eval-read "#U{-"))
    (signals (end-of-file) (eval-read "#U{-a"))
    (signals (invalid-expansion-error) (eval-read "#U{-opt|b|c=,}"))
    (signals (invalid-expansion-error) (eval-read "#U{%"))))


(test (uri-warnings :suite disabled-tests)
  (with-fixture uri-template-syntax ()
    (signals (invalid-uri-warning) (eval-read "#Uaa}"))
    (signals (invalid-uri-warning) (eval-read "#U<>"))
    (signals (invalid-uri-warning) (eval-read "#U%"))
    (signals (invalid-uri-warning) (eval-read "#U%1"))
    (signals (invalid-uri-warning) (eval-read "#U%1g"))
    (signals (invalid-uri-warning) (eval-read "#U%a"))
    (signals (invalid-uri-warning) (eval-read "#U%ag"))
    (signals (invalid-uri-warning) (eval-read "#U%ga"))
    (signals (invalid-uri-warning) (eval-read "#U%gg"))))


(def-suite variables :in expansions)
(in-suite expansions)


(define-fixture some-variables ()
  (let ((baz 1)
        (bar "bar"))
    (declare (ignorable bar baz))
    (&body)))


(test invalid-variables
  (signals invalid-var-error (eval '(expand-uri-template "{}")))
  (signals invalid-var-error (eval '(expand-uri-template "{@}")))
  (signals invalid-var-error (eval '(expand-uri-template "{{}")))
  (signals invalid-var-error (eval '(expand-uri-template "{ }")))
  (signals invalid-var-error (eval '(expand-uri-template "{a@}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a{}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a }")))
  (signals invalid-var-error (eval '(expand-uri-template "{@a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{{a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{ a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a@a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a{a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a a}")))
  (signals invalid-var-error (eval '(expand-uri-template "a{a@a}a")))
  (signals invalid-var-error (eval '(expand-uri-template "a{a{a}a")))
  (signals invalid-var-error (eval '(expand-uri-template "a{a a}a")))
  (signals invalid-var-error (eval '(expand-uri-template "{=}")))
  (signals invalid-var-error (eval '(expand-uri-template "{={}")))
  (signals invalid-var-error (eval '(expand-uri-template "{=a}")))
  (signals invalid-var-error (eval '(expand-uri-template "{|}")))
  (signals invalid-var-error (eval '(expand-uri-template "{a={}")))
  (signals invalid-var-error (eval '(expand-uri-template "{.abc}")))
  (signals invalid-var-error (eval '(expand-uri-template "{+4abc}")))
  (signals invalid-var-error (eval '(expand-uri-template "{4a:bc}")))
  (signals invalid-var-error (eval '(expand-uri-template "{abc}{_abc}"))))


(test variable-substitution
  "Successful variable substitutions"
  (is (string= "fredfredfred"
               (let ((foo "fred"))
                 (expand-uri-template "{foo}{foo=}{foo=wilma}"))))
  (is (string= "wilma"
               (expand-uri-template "{bar=wilma}")))
  (is (string= ""
               (expand-uri-template "{baz}{1baz}{123}{1-2}{1-baz}{ba.z_1-2}")))
  (is (string= "foo..baz.qux."
               (let ((f.oo_ "foo")
                     (b-a-r nil)
                     (b-a.z_ "baz"))
                 (expand-uri-template "{f.oo_}.{b-a-r}.{b-a.z_=nn}.{4q.-_x=qux}."))))
  (is (string= "http://example.org/?q=fred"
               (let ((bar "fred"))
                 (expand-uri-template "http://example.org/?q={bar}"))))
  (is (string= "http://www.foo.com/bar/1"
               (let ((baz 1))
                 (expand-uri-template "http://www.foo.com/bar/{baz}"))))
  (is (string= "http://www.foo.com/bar/bar/1"
               (let ((bar "bar")
                     (baz 1))
                 (expand-uri-template "http://www.foo.com/bar/{bar}/{baz}"))))
  (is (string= "http://www.foo.com/bar/bar1"
               (let ((bar "bar")
                     (baz 1))
                 (expand-uri-template "http://www.foo.com/bar/{bar}{baz}"))))
  (with-fixture uri-template-syntax ()
    (is (string= "...1.wil.."
                 (eval-read "(let (foo bar (baz 1) (qux \"wil\"))
                               #U.{foo}.{bar=wilma}.{baz}.{qux=wilma}.{flub}.)")))))


(def-suite operators :in expansions)
(in-suite operators)


(test operator-opt
  "Operator -opt"
  (is (string= ""
               (expand-uri-template "{-opt||foo}{-opt|bar|foo}")))
  (is (string= "..bar."
               (let (foo (bar 1))
                 (expand-uri-template ".{-opt||foo}.{-opt|bar|bar}."))))
  (is (string= ".foo.bar."
               (let ((foo ""))
                 (expand-uri-template ".{-opt|foo|foo}.{-opt|bar|bar=1}."))))
  (signals invalid-op-vars-error (parse-uri-template "{-opt||foo,bar}"))
  (with-fixture uri-template-syntax ()
    (is (string= ".foo."
                 (eval-read "(let ((foo 1))
                               #U.{-opt|foo|foo}.)")))))


(test operator-neg
  "Operator -neg"
  (is (string= "..bar."
               (expand-uri-template ".{-neg||foo}.{-neg|bar|foo}.")))
  (is (string= ".foo.."
               (expand-uri-template ".{-neg|foo|foo}.{-neg|bar|bar=}.")))
  (is (string= "..."
               (let (foo (bar 1))
                 (expand-uri-template ".{-neg||foo}.{-neg|bar|bar}."))))
  (is (string= "..."
               (let ((foo ""))
                 (expand-uri-template ".{-neg|foo|foo}.{-neg|bar|bar=1}."))))
  (signals invalid-op-vars-error (parse-uri-template "{-neg||foo,bar}"))
  (with-fixture uri-template-syntax ()
    (is (string= "..bar."
                 (eval-read "(let ((foo 1))
                               #U.{-neg|foo|foo}.{-neg|bar|bar}.)")))))


(test operator-prefix
  "Operator -prefix"
  (is (string= "..bar."
               (expand-uri-template ".{-prefix||foo}.{-prefix|bar|foo}.")))
  (is (string= ".foo.bar."
               (expand-uri-template ".{-prefix|foo|foo}.{-prefix|bar|bar=}.")))
  (is (string= ".a.bar1."
               (let (foo (bar 1))
                 (expand-uri-template ".{-prefix|a|foo}.{-prefix|bar|bar}."))))
  (is (string= "./.+1."
               (let ((foo ""))
                 (expand-uri-template ".{-prefix|/|foo}.{-prefix|+|bar=1}."))))
  (is (string= ".abcd123.+a+b+c+d+1+2++3."
               (let ((foo '("a" "b" "c" "d" 1 2 nil 3)))
                 (expand-uri-template ".{-prefix||foo}.{-prefix|+|foo=1}."))))
  (signals invalid-var-error (parse-uri-template "{-prefix||}"))
  (signals invalid-op-vars-error (parse-uri-template "{-prefix||foo,bar}"))
  (with-fixture uri-template-syntax ()
    (is (string= ".foo1.bar"
                 (eval-read "(let ((foo 1))
                               #U.{-prefix|foo|foo}.{-prefix|bar|bar})")))))


(test operator-suffix
  "Operator -suffix"
  (is (string= "..bar."
               (expand-uri-template ".{-suffix||foo}.{-suffix|bar|foo}.")))
  (is (string= ".foo.bar."
               (expand-uri-template ".{-suffix|foo|foo}.{-suffix|bar|bar=}.")))
  (is (string= "..1bar."
               (let (foo (bar 1))
                 (expand-uri-template ".{-suffix||foo}.{-suffix|bar|bar}."))))
  (is (string= "./.1+."
               (let ((foo ""))
                 (expand-uri-template ".{-suffix|/|foo}.{-suffix|+|bar=1}."))))
  (is (string= ".abcd123.a+b+c+d+1+2++3+."
               (let ((foo '("a" "b" "c" "d" 1 2 nil 3)))
                 (expand-uri-template ".{-suffix||foo}.{-suffix|+|foo=1}."))))
  (signals invalid-var-error (parse-uri-template "{-suffix||}"))
  (signals invalid-op-vars-error (parse-uri-template "{-suffix||foo,bar}"))
  (with-fixture uri-template-syntax ()
    (is (string= ".1foo.bar."
                 (eval-read "(let ((foo 1))
                               #U.{-suffix|foo|foo}.{-suffix|bar|bar}.)")))))


(test operator-join
  "Operator -join"
  (is (string= ".foo=.foo=."
               (expand-uri-template ".{-join||foo}.{-join|bar|foo}.")))
  (is (string= ".foo=&bar=."
               (expand-uri-template ".{-join|&|foo,bar=}.")))
  (is (string= ".foo=&bar=1."
               (let (foo (bar 1))
                 (expand-uri-template ".{-join|&|foo,bar}."))))
  (is (string= ".foo=/bar=.foo=+bar=1."
               (let ((foo ""))
                 (expand-uri-template ".{-join|/|foo,bar}.{-join|+|foo,bar=1}."))))
  (is (string= "a=a,b=b,c=,d=d,e=1,f=2,g=3"
               (let ((a "a") (b "b") (d "d") (e 1) (f 2))
                 (expand-uri-template "{-join|,|a,b,c,d,e,f,g=3}"))))
  (is (string= "a=a&b=b&c=c&d=d&e=1&f=2&g=3"
               (let ((a "a") (b "b") (d "d") (e 1) (f 2) (g 3))
                 (expand-uri-template "{-join|&|a,b,c=c,d=1,e,f,g}"))))
  (signals invalid-var-error (parse-uri-template "{-join||}"))
  (with-fixture uri-template-syntax ()
    (is (string= ".foo=1.foo=1+++bar=b"
                 (eval-read "(let ((foo 1))
                               #U.{-join|foo|foo}.{-join|+++|foo,bar=b})")))))


(test operator-list
  "Operator -list"
  (is (string= "..."
               (expand-uri-template ".{-list||foo}.{-list|bar|foo}.")))
  (is (string= "..1."
               (let (foo (bar '(1)))
                 (expand-uri-template ".{-list||foo}.{-list|bar|bar}."))))
  (is (string= "./.++1+2."
               (let ((foo '(nil nil)) (bar '(nil "" 1 2)))
                 (expand-uri-template ".{-list|/|foo}.{-list|+|bar}."))))
  (is (string= ".abcd.a+b+c+d."
               (let ((foo '("a" "b" "c" "d")))
                 (expand-uri-template ".{-list||foo}.{-list|+|foo}."))))
  (signals invalid-var-error (parse-uri-template "{-list||}"))
  (signals invalid-op-vars-error (expand-uri-template
                                  ".{-list|foo|foo}.{-list|bar|bar=}."))
  (with-fixture uri-template-syntax ()
    (is (string= ".1.1,+++,b"
                 (eval-read "(let ((foo '(1)) (bar '(1 +++ #\\b)))
                               #U.{-list|foo|foo}.{-list|,|bar})")))))


(def-suite destructuring :in enabled-tests)
(in-suite destructuring)


(test destructuring-vars
  "Destructuring variables"
  (is (string= ""
               (with-destructured-uri "" "{foo}" ()
                 (uri-var 'foo))))
  (is (string= ""
               (with-destructured-uri "" "{foo=bar}" (foo)
                 foo)))
  (is (string= ""
               (with-destructured-uri "" "{foo}{bar}" (foo bar)
                 foo)))
  (is (string= "fOo"
               (with-destructured-uri "fOo" "{foo}" ()
                 (uri-var 'foo))))
  (is (string= "fOo"
               (with-destructured-uri ".fOo" ".{foo}" ()
                 (uri-var 'foo))))
  (is (string= "fOo"
               (with-destructured-uri "fOo." "{foo}." ()
                 (uri-var 'foo))))
  (is (string= "BaAr"
               (with-destructured-uri "fooBaArbaz" "foo{bar}baz" (bar)
                 bar)))
  (is (equal '("BaAr" "bAz")
             (with-destructured-uri "fooBaAr/bAz" "foo{bar}/{baz}" (bar baz)
               (list bar baz))))
  (is (equal '("BaAr" "bAz")
             (with-destructured-uri "fooBaAr/bAz" "foo{bar}/{baz}" ()
               (mapcar #'uri-var '(bar baz)))))
  (is (equal '("" "foobar")
             (with-destructured-uri "foobar" "{foo}{bar}" (foo bar)
               (list foo bar)))))


(def-suite run-time :in enabled-tests)
(in-suite run-time)


(test run-time-expansion
  "Expansions with run-time variables"
  (is (equal ""
             (with-destructured-uri "" "{foo}" ()
               (expand-uri-template "{foo}"))))
  (is (equal "foo"
             (with-destructured-uri "foo" "{foo}" ()
               (expand-uri-template "{foo}"))))
  (is (equal "foo+bar"
             (with-destructured-uri "foo-bar" "{foo}-{bar}" ()
               (expand-uri-template "{foo}+{bar}"))))
  (is (equal "?foo=a&bar=b"
             (with-destructured-uri "a/b" "{foo}/{bar}" (foo bar)
               (expand-uri-template "?{-join|&|foo,bar}"))))
  (is (equal "?foo=a&bar=b"
             (with-destructured-uri "a/b" "{foo}/{bar}" ()
               (expand-uri-template "?{-join|&|foo,bar}"))))
  (is (equal "foo"
             (let ((foo 2))
               (with-destructured-uri "foo" "{foo}" ()
                 (expand-uri-template "{foo}"))))))


(with-open-file (*standard-output* "test.output" :direction :output
                                   :if-exists :supersede)
  (time (run! 'enabled-tests)))
