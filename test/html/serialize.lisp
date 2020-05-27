(in-package :html-test)

(in-suite :html-test)

(test serialize
  (is (equal "<div class=\"foo\">bar</div>"
             (html:serialize (html:div :class "foo" "bar"))))

  (is (equal "<div></div>"
             (html:serialize (html:div :class nil))))

  (is (equal "<div foo></div>"
             (html:serialize (html:div :foo t))))

  (is (equal "<div foo></div>"
             (html:serialize (html:div :foo ""))))

  (is (equal "<div foo=\"42\"></div>"
             (html:serialize (html:div :foo 42)))))

(test serialize/svg
  (is (equal "<div><svg width=\"100\" height=\"100\"><circle cx=\"50\" cy=\"50\" r=\"40\"></circle></svg></div>"
             (html:serialize (html:div
                              (svg:svg :width "100" :height "100"
                               (svg:circle :cx "50" :cy "50" :r "40")))))))