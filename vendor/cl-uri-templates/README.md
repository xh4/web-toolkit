PROJECT
-------

[CL-URI-TEMPLATES][1] is an extensive Common-Lisp implementation
of the URI-Templates draft proposed to the W3C.

It is currently developped by [Thomas de Grivel][2]

 [1]: http://github.com/billitch/cl-uri-templates
 [2]: mailto:billitch@gmail.com


USAGE
-----

If you want simple expansion of a template string using the current
lexical environment, use `EXPAND-URI-TEMPLATE`.

There is also a reader macro parsing forms like `#U{foo}/{bar}` which
you can enable with `ENABLE-URI-TEMPLATE-SYNTAX`.


Destructuring URIs according to a template is provided by
`WITH-DESTRUCTURED-URI`, or if you want more persistent variables you
can bind `*URI-ENVIRONMENT*` and use `DESTRUCTURE-URI` to populate it.


When writing URI templates please note that some expansions do not
have reciprocal deconstructions, for instance

    (equal '("" "foobar")
           (with-destructured-uri "foobar" "{foo}{bar}" (foo bar)
              (list foo bar)))


PROGRESS
--------

Standard (draft) is fully implemented. Templates can be parsed at
run-time, expansion of URIs with variables and operators is fully
supported.

Beyond standard, destructuring URIs works with simple variables,
destructuring templates using operators is under development.

Provided tests should all pass, they are run with test.sh or test.lisp
Please report any bug you encounter, test cases and patches are also
highly appreciated.


REFERENCE
---------

This implementation is based on the [URI-Templates draft][1] proposed by
Joe Gregorio to the W3C.

 [1]: http://bitworking.org/projects/URI-Templates/


HISTORY
-------

This package is originally a fork from [uri-template][1] from Vladimir Sedach.

Goal was to contribute the missing operator parsers, and
further extend the URI deconstruction with these same operators.

Moreover run-time destructuring of URI was added, previous code
would only allow uri-templates as a reader macro.

 [1]: http://common-lisp.net/project/uri-template/


LICENSE
-------

For license and copyright information (BSD), see the file COPYING
included with the distribution.
