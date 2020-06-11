(in-package :html-test)

(in-suite :html-test)

(test parse
  (it
    (let ((document (parse "<h1>Title</h1>")))
      (let ((h1 (first (dom:get-elements-by-tag-name document "h1"))))
        (is (eq 'html:h1 (type-of h1)))
        (is (equal "Title" (dom:text-content h1)))))))

(test parse/svg
  (it
    (let ((document (parse "<svg></svg>")))
      (let ((svg (first (dom:get-elements-by-tag-name document "svg"))))
        (is-true (typep svg 'svg:svg))))))