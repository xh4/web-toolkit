#! /bin/sh

rm *.fasl
mv test.output test.output~
sbcl --script test.lisp && \
    cat test.output
