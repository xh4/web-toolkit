(in-package :uri-test)

(in-suite :uri-test)

(test uri-parsing
  )

(test uri-constructing
  (it
    (let ((uri (uri "http://example.com" :port 80)))
      (is (equal 80 (uri-port uri)))))

  ;; FIXME: failed, don't know why...
  ;; (it
  ;;   (let ((uri (uri "http://example.com" :query '(("foo" . "bar")))))
  ;;     (is (equal "foo=bar" (uri-query uri)))))
  )
