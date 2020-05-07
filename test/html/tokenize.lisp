(in-package :html-test)

(in-suite :html-test)

(test tokenize/doctype
  (it
    (let ((token (first (html::tokenize "<!doctype html>"))))
      (is (eq 'html::doctype-token (type-of token)))
      (is (equal "html" (slot-value token 'html::name)))))

  (it
    (let ((token (first (html::tokenize "<!DOCTYPE html>"))))
      (is (eq 'html::doctype-token (type-of token)))
      (is (equal "html" (slot-value token 'html::name))))))

(test tokenize/named-character-reference
  (it
    (let ((token (first (html::tokenize "&lt;"))))
      (is (eq #\< token))))

  (it
    (signals error (html::tokenize "I'm &notit; I tell you")))

  (it
    (handler-bind ((html::parse-error (lambda (c) (continue))))
      (let ((chars (html::tokenize "I'm &notit; I tell you")))
        (is (equal "I'm ¬it; I tell you" (coerce chars 'string))))))

  (it
    (finishes (html::tokenize "I'm &notin; I tell you")))

  (it
    (let ((chars (html::tokenize "I'm &notin; I tell you")))
      (is (equal "I'm ∉ I tell you" (coerce chars 'string))))))
