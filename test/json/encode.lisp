(in-package :json-test)

(in-suite :json-test)

(test encode/json-string()
   (is (string= (encode (format nil "hello~&hello"))
                 "\"hello\\nhello\""))
   (is (string= (encode (format nil "\"aquote"))
                 "\"\\\"aquote\"")))

(test encode/json-literals
  (is (string= "true" (encode t)))
  (is (string= "false" (encode nil))))

(defun is-same-number (nr)
  "If it gets decoded back ok then it was encoded ok"
  (is (= nr (decode (encode nr)))))

(test encode/json-number
  (is (string= "0" (encode 0)))
  (is (string= "13" (encode 13)))
  (is (string= "13.02" (encode 13.02)))
  (is (string= "13.02" (encode 13.02D0)))
  (is (string= "13.02" (encode 13.02L0)))
  (is (string= "13.02" (encode 13.02S0)))
  (is (string= "13.02" (encode 13.02E0)))

  (is-same-number 2e10)
  (is-same-number -1.3234e-10)
  (is-same-number -1280.12356)
  (is-same-number 1d2)
  (is-same-number 1l2)
  (is-same-number 1s2)
  (is-same-number 1f2)
  (is-same-number 1e2))

(defun decode-then-encode (json)
  (assert (member (elt json 0) '(#\{ #\[ #\" ))) ; must be json
  (flet ((normalize (string)
           (remove #\Return (remove #\Newline (remove #\Space string)))))
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

(test encode/controls
   (decode-then-encode "\"\\\\b\\\\f\\\\n\\\\r\\\\\""))

(test encode/slash
  (let* ((z "\"/ & /\"")
         (also-z "\"/ & \/\"") ;Extra quote
         (x (encode z))
         (also-x (encode also-z))
         (y (decode x))
         (also-y (decode also-x)))
    (is (string= x also-x))
    (is (string= y also-y))
    (is (string= z y))))

(test encode/quoted
  (decode-then-encode "\"&#34; %22 0x22 034 &#x22;\""))

(test encode/alpha-1
  (decode-then-encode "\"abcdefghijklmnopqrstuvwyz\""))

(test encode/alpha-2
  (decode-then-encode "\"ABCDEFGHIJKLMNOPQRSTUVWYZ\""))

(test encode/digit
  (decode-then-encode "\"0123456789\""))

(test encode/special
  (decode-then-encode "\"`1~!@#$%^&*()_+-={':[,]}|;.<>?\""))

(test encode/hex
  (decode-then-encode "\"\u0123\u4567\u89AB\uCDEF\uabcd\uef4A\""))

(test encode/true
  (decode-then-encode "[ true]"))

(test encode/false
  (is (string= "[false]" (encode (decode "[false]")))))

(test encode/null
  (decode-then-encode "[null]"))

(test encode/array
  (is (string= "[]" (encode (decode "[  ]"))))
  (is (string= "[1,2,3,true]" (encode `(1 2 3 t))))
  (is (string= "[1,2,3,[]]" (encode `(1 2 3 nil))))
  (is (string= "[1,2,3,{\"foo\":false}]"
               (encode `(1 2 3 ,(object "foo" nil))))))

(test encode/character
  ;;Characters are encoded to strings, but when decoded back to string
  (is (string= (encode #\a) "\"a\"")))
