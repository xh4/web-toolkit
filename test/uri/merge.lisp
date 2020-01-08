(in-package :uri-test)

(in-suite :uri-test)

(test remove-dot-segments
  (is (equal "/a/g" (uri::remove-dot-segments "/a/b/c/./../../g")))
  (is (equal "mid/6" (uri::remove-dot-segments "mid/content=5/../6"))))

(defmacro test-merge-uri (&body forms)
  (let ((base-uri "http://a/b/c/d;p?q"))
    `(test merge-uri
       ,@(loop for (target-uri relative-uri) in forms
            collect `(is (equal ,target-uri
                                (uri-string
                                 (merge-uri ,base-uri ,relative-uri))))))))

(test-merge-uri
 ("g:h" "g:h")
 ("http://a/b/c/g" "g")
 ("http://a/b/c/g" "./g")
 ("http://a/b/c/g/" "g/")
 ("http://a/g" "/g")
 ("http://g" "//g")
 ("http://a/b/c/d;p?y" "?y")
 ("http://a/b/c/g?y" "g?y")
 ("http://a/b/c/d;p?q#s" "#s")
 ("http://a/b/c/g#s" "g#s")
 ("http://a/b/c/g?y#s" "g?y#s")
 ("http://a/b/c/;x" ";x")
 ("http://a/b/c/g;x" "g;x")
 ("http://a/b/c/g;x?y#s" "g;x?y#s")
 ("http://a/b/c/d;p?q" "")
 ("http://a/b/c/" ".")
 ("http://a/b/c/" "./")
 ("http://a/b/" "..")
 ("http://a/b/" "../")
 ("http://a/b/g" "../g")
 ("http://a/" "../..")
 ("http://a/" "../../")
 ("http://a/g" "../../g"))
