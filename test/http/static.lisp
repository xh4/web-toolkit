(in-package :http-test)

(in-suite :http-test)

(test path-trim-prefix
  (is (equal "" (http::path-trim-prefix "foo" "/foo")))
  (is (equal "" (http::path-trim-prefix "foo" "/foo/")))
  (is (equal "bar" (http::path-trim-prefix "foo" "/foo/bar")))
  (is (equal "bar" (http::path-trim-prefix "/foo" "/foo/bar")))
  (is (equal "bar" (http::path-trim-prefix "foo/" "/foo/bar")))
  (is (equal "bar" (http::path-trim-prefix "/foo/" "/foo/bar")))
  (is (equal "" (http::path-trim-prefix "/foo/bar" "/foo/bar")))
  (is (equal "" (http::path-trim-prefix "/foo/bar" "/foo/bar/")))
  (is (equal "" (http::path-trim-prefix "/foo/bar/" "/foo/bar")))
  (is (equal "" (http::path-trim-prefix "/foo/bar/" "/foo/bar/")))
  (is (equal nil (http::path-trim-prefix "goo" "/foo/bar/")))
  (is (equal "foo/bar/" (http::path-trim-prefix "/" "/foo/bar/")))
  (is (equal "foo/bar" (http::path-trim-prefix "" "/foo/bar")))
  (is (equal "foo/bar/" (http::path-trim-prefix nil "/foo/bar/"))))
