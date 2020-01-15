(in-package :json-test)

(in-suite :json-test)

(test json-string()
   (is (string= (encode (format nil "hello~&hello"))
                 "\"hello\\nhello\""))
   (is (string= (encode (format nil "\"aquote"))
                 "\"\\\"aquote\"")))

(test json-literals
  (is (string= "true" (encode t)))
  (is (string= "null" (encode nil))))

(defun is-same-number(nr)
  "If it gets decoded back ok then it was encoded ok"
  (is (= nr (decode (encode nr)))))

(test json-number
  (is (string= "0" (encode 0)))
  (is (string= "13" (encode 13)))
  (is (string= "13.02" (encode 13.02)))
  (is (string= "13.02" (encode 13.02D0)))
  (is (string= "13.02" (encode 13.02L0)))
  (is (string= "13.02" (encode 13.02S0)))
  (is (string= "13.02" (encode 13.02E0)))

  (is-same-number 2e10)
  (is-same-number  -1.3234e-10)
  (is-same-number -1280.12356)
  (is-same-number 1d2)
  (is-same-number 1l2)
  (is-same-number 1s2)
  (is-same-number 1f2)
  (is-same-number 1e2))

(defun decode-then-encode (json)
  (assert (member (elt json 0) '(#\{ #\[ #\" ))) ;must be json
  (flet ((normalize (string)
           (remove #\Newline (remove #\Space string))))
    (let* ((decoded (decode json))
           (encoded (encode decoded)))
      ;;        (format t "Decoded:~a~&" decoded)
      ;;        (format t "Encoded:~a" encoded)
      (is (string= (normalize json)
                   (normalize encoded))))))

(test encode-pass-2
  (decode-then-encode "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))

(test encode-pass-3
  (decode-then-encode "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }
}
"))

(test controls
   (decode-then-encode "\"\\\\b\\\\f\\\\n\\\\r\\\\\""))

(test slash
  (let* ((z "\"/ & /\"")
         (also-z "\"/ & \/\"") ;Extra quote
         (x (encode z))
         (also-x (encode also-z))
         (y (decode x))
         (also-y (decode also-x)))
    (is (string= x also-x))
    (is (string= y also-y))
    (is (string= z y))))


(test quoted
  (decode-then-encode "\"&#34; %22 0x22 034 &#x22;\""))

(test alpha-1
  (decode-then-encode "\"abcdefghijklmnopqrstuvwyz\""))

(test alpha-2
  (decode-then-encode "\"ABCDEFGHIJKLMNOPQRSTUVWYZ\""))

(test digit
  (decode-then-encode "\"0123456789\""))

(test special
  (decode-then-encode "\"`1~!@#$%^&*()_+-={':[,]}|;.<>?\""))

(test hex
  (decode-then-encode "\"\u0123\u4567\u89AB\uCDEF\uabcd\uef4A\""))

(test true
  (decode-then-encode "[ true]"))

(test false
  (is (string= (encode (decode "[false]"))
               "[null]")));;We dont separate between false and null
(test null
  (decode-then-encode "[null]"))

(test array
  ;;Since empty lists becomes nil in lisp, they are converted back to null
  (is (string= (encode (decode "[  ]"))
               "null")))

(test character
  ;;Characters are encoded to strings, but when decoded back to string
  (is (string= (encode #\a) "\"a\"")))


(test hash-table-symbol
  (let ((ht (make-hash-table)))
    (setf (gethash 'symbols-are-now-converted-to-camel-case ht) 5)
    (is (string= (encode ht)
                 "{\"symbolsAreNowConvertedToCamelCase\":5}"))))

(test hash-table-string
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "lower x" ht) 5)
    (is (string= (encode ht)
                 "{\"lower x\":5}"))))

(test json-bool
  (is (equal (json::json-bool t) '(:true)))
  (is (equal (json::json-bool 1) '(:true)))
  (is (equal (json::json-bool nil) '(:false))))

(test json-or-null
  (is (equal (json::json-or-null 'something) 'something))
  (is (equal (json::json-or-null '(:list)) '(:list)))
  (is (equal (json::json-or-null nil) '(:null)))
  (is (equal (json::json-or-null (when t  '(:list)))
             '(:list)))
  (is (equal (json::json-or-null (when nil  '(:list)))
             '(:null))))
