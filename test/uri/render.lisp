(in-package :uri-test)

(in-suite :uri-test)

(test uri-string
  (it
    (let ((uri (uri :query '("q" "爱"))))
      (is (equal "q=%E7%88%B1" (uri-string uri))))

    (equal "foo=bar" (uri-string :query '("foo" "bar")))))
