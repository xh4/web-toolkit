(in-package :component-test)

(in-suite :component-test)

(test diff
  (is (equal nil (com::diff (html:div) (html:div))))
  (is (equal nil (com::diff (html:div :class "foo") (html:div :class "foo"))))
  (is (equal nil (com::diff (html:div :class "foo bar") (html:div :class "bar foo")))))
