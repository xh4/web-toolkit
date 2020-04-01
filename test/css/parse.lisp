(in-package :css-test)

(in-suite :css-test)

(test consume-comments
  (it
    (tokenize-string (tokenizer "/* abc */d")
      (is (equal nil (css::consume-comments tokenizer)))
      (is (equal #\d (css::consume-code-point tokenizer))))

    (tokenize-string (tokenizer "/* abc *")
      (signals error (css::consume-comments tokenizer)))

    (tokenize-string (tokenizer "abc")
      (css::consume-comments tokenizer)
      (is (equal #\a (css::consume-code-point tokenizer))))))

(test consume-whitespace
  (it
    (tokenize-string (tokenizer "   abc")
      (css::consume-whitespace tokenizer)
      (is (equal #\a (css::consume-code-point tokenizer)))))

  (it
    (tokenize-string (tokenizer "abc")
      (css::consume-whitespace tokenizer)
      (is (equal #\a (css::consume-code-point tokenizer))))))

(test consume-escaped-code-point
  (it
    (tokenize-string (tokenizer "41")
      (is (equal #\A (css::consume-escaped-code-point tokenizer))))))

(test consume-string
  (it
    (tokenize-string (tokenizer "\"abc\"")
      (css::consume-code-point tokenizer)
      (is (equal '(:string "abc") (css::consume-string tokenizer))))))
