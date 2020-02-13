(in-package :http-test)

(in-suite :http-test)

(test file-size
  (it
    (with-static-files (root "abc")
      (with-open-file (stream (merge-pathnames "abc" root)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence (make-array 42 :initial-element 42) stream))
      (let ((file (make-instance 'http::file
                                 :pathname (merge-pathnames "abc" root))))
        (is (equal 42 (http::file-size file)))))))

(test file-size/not-exists
  (it
    (with-static-files (root)
      (let ((file (make-instance 'http::file
                                 :pathname (merge-pathnames "xxx" root))))
        (is (equal nil (http::file-size file)))))))
