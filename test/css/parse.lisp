(in-package :css-test)

(in-suite :css-test)

(test consume-code-point
  (it
    (tokenize-string (tokenizer "abc")
      (is (equal #\a (css::consume-code-point tokenizer)))
      (is (equal #\b (css::consume-code-point tokenizer)))
      (is (equal #\c (css::consume-code-point tokenizer)))
      (is (equal nil (css::consume-code-point tokenizer))))))

(test next-input-code-point
  (it
    (tokenize-string (tokenizer "abcd")
      (is (equal #\a (css::consume-code-point tokenizer)))
      (is (equal #\b (css::next-input-code-point tokenizer)))
      (is (equal "bc" (css::next-2-input-code-points tokenizer)))
      (is (equal "bcd" (css::next-3-input-code-points tokenizer))))))

(test reconsume-current-input-code-point
  (it
    (tokenize-string (tokenizer "abc")
      (is (equal #\a (css::consume-code-point tokenizer)))
      (css::reconsume-current-input-code-point tokenizer)
      (is (equal #\a (css::consume-code-point tokenizer)))))

  (it
    (tokenize-string (tokenizer "abc")
      (is (equal #\a (css::consume-code-point tokenizer)))
      (css::reconsume-current-input-code-point tokenizer)
      (css::next-2-input-code-points tokenizer)
      (is (equal #\a (css::consume-code-point tokenizer)))))

  (it
    (tokenize-string (tokenizer "abc")
      (is (equal #\a (css::consume-code-point tokenizer)))
      (css::next-2-input-code-points tokenizer)
      (css::reconsume-current-input-code-point tokenizer)
      (is (equal #\a (css::consume-code-point tokenizer))))))

(test consume-comments
  (it
    (tokenize-string (tokenizer "/* abc */d")
      (is (equal nil (css::consume-comments tokenizer)))
      (is (equal #\d (css::consume-code-point tokenizer)))))

  (it
    (tokenize-string (tokenizer "/* abc *")
      (signals error (css::consume-comments tokenizer))))

  (it
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
      (is (equal '(:string "abc") (css::consume-string tokenizer)))))

  (it
    (tokenize-string (tokenizer "'abc'")
      (css::consume-code-point tokenizer)
      (is (equal '(:string "abc") (css::consume-string tokenizer))))))
