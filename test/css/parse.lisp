(in-package :css-test)

(in-suite :css-test)

(test parse-declaration
  (it
   (with-input-from-string (stream "background: red")
     (let ((parser (make-instance 'css::parser :stream stream)))
       (let ((declaration (css::parse-declaration parser)))
         (is (equal 'declaration (type-of declaration)))
         (is (equal "background" (declaration-name declaration)))
         (is-true (listp (declaration-value declaration)))
         (is (equal 'ident-token (type-of (first (declaration-value declaration)))))
         (is (equal "red" (ident-token-value (first (declaration-value declaration))))))))))

(test parse-list-of-declarations
  (it
   (with-input-from-string (stream "background: red; margin: 10px 20px;")
     (let ((parser (make-instance 'css::parser :stream stream)))
       (let ((declarations (css::parse-list-of-declarations parser)))
         (is (equal 2 (length declarations)))
         (is (equal "background" (declaration-name (first declarations))))
         (is (equal "margin" (declaration-name (second declarations)))))))))

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

(test parse-list-of-declarations-from-rule
  (it
    (with-input-from-string (stream ".foo, .bar { background: red; margin: 10px 20px; }")
      (let ((parser (make-instance 'css::parser :stream stream)))
        (let ((rule (css::parse-rule parser)))
          (let ((declarations (css::parse-list-of-declarations (rule-block rule))))
            (is (equal 2 (length declarations)))
            (is (equal "background" (declaration-name (first declarations))))
            (is (equal "margin" (declaration-name (second declarations))))))))))
