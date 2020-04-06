(in-package :css-test)

(in-suite :css-test)

(test parse-declaration
  (it
   (with-input-from-string (stream "background: red")
     (let* ((parser (make-instance 'css::parser :stream stream)))
       (css::parse-declaration parser)))))

(test parse-list-of-declarations
  (it
   (with-input-from-string (stream "background: red; margin: 10px 20px;")
     (let* ((parser (make-instance 'css::parser :stream stream)))
       (css::parse-list-of-declarations parser)))))

(test parse-qualified-rule
  (it
   (with-input-from-string (stream ".foo, .bar { background: red; margin: 10px 20px; }")
     (let* ((parser (make-instance 'css::parser :stream stream)))
       (css::parse-rule parser)))))

(test parse-at-rule
  (it
   (with-input-from-string (stream "@import \"my-styles.css\"")
     (let* ((parser (make-instance 'css::parser :stream stream)))
       (css::parse-rule parser)))))
