(in-package :css-test)

(in-suite :css-test)

(test parse-declaration
  (it
   (with-input-from-string (stream "background: red")
     (let ((parser (make-instance 'css::parser :stream stream)))
       (let ((declaration (css::parse-declaration parser)))
         (is (equal 'declaration (type-of declaration)))
         (is (equal "background" (declaration-name declaration)))
         (is (equal "red" (declaration-value declaration))))))))

(test parse-list-of-declarations
  (it
   (with-input-from-string (stream "background: red; margin: 10px 20px;")
     (let ((parser (make-instance 'css::parser :stream stream)))
       (let ((declarations (css::parse-list-of-declarations parser)))
         (is (equal 2 (length declarations)))
         (is (equal "background" (declaration-name (first declarations))))
         (is (equal "red" (declaration-value (first declarations))))
         (is (equal "margin" (declaration-name (second declarations))))
         (is (equal "10px 20px" (declaration-value (second declarations)))))))))

(test parse-qualified-rule
  (it
   (with-input-from-string (stream ".foo, .bar { background: red; margin: 10px 20px; }")
     (let ((parser (make-instance 'css::parser :stream stream)))
       (let ((rule (css::parse-rule parser)))
         (is (equal 'qualified-rule (type-of rule))))))))

(test parse-at-rule
  (it
   (with-input-from-string (stream "@import \"my-styles.css\"")
     (let ((parser (make-instance 'css::parser :stream stream)))
       (let ((rule (css::parse-rule parser)))
         (is (equal 'at-rule (type-of rule))))))))
