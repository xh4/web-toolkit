(in-package :javascript-test)

(in-suite :javascript-test)

(test scan-string-literal
  (it
   (let ((scanner (make-instance 'scanner :source "'foo'")))
     (let ((token (js::scan-string-literal scanner)))
       (is (equal 'token (type-of token)))
       (is (equal :string-literal (token-type token)))
       (is (equal "foo" (token-value token)))))))