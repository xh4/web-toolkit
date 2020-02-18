(in-package :http-test)

(in-suite :http-test)

(test read-until-next-boundary
  (it
    (with-input-from-lines (stream '("--boundary"
                                     "Content-Disposition: form-data; name=\"foo\""
                                     ""
                                     "foo"
                                     "--boundary--"))
      (multiple-value-bind (part not-last)
          (http::read-until-next-boundary stream "boundary")
        (is-true (= 0 (length part)))
        (is-true not-last))
      (multiple-value-bind (part not-last)
          (http::read-until-next-boundary stream "boundary")
        (is-true (< 0 (length part)))
        (is-false not-last))
      (signals end-of-file (read-byte stream)))))

(test read-part-header
  (it
    (with-input-from-lines (stream '("Content-Disposition: form-data; name=\"foo\"; filename=\"bar\""
                                     "Content-Type: text/plain; charset=UTF-8"
                                     ""))
      (let ((header (http::read-part-header stream)))
        (is (equal "foo" (getf header :name)))
        (is (equal "bar" (getf header :filename)))
        (is (equal "text/plain; charset=UTF-8" (getf header :content-type)))))))

(test read-multipart-form-data
  (it
    (with-input-from-form-data (stream boundary (http::form "foo" "bar"))
      (let ((alist (http::read-multipart-form-data stream boundary)))
        (is (equal "foo" (caar alist))
            (equal "bar" (babel:octets-to-string (cdar alist))))))))
