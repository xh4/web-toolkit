(in-package :http-test)

(in-suite :http-test)

(test stream
  (babel-streams:with-input-from-sequence (stream #(1 2 3 4 5))
    (setf stream (make-instance 'http::stream :upstream stream :length 5))

    (is (= 1 (read-byte stream)))
    (is (= 4 (http::stream-remaining-length stream)))

    (read-sequence (make-array 2) stream)
    (is (= 2 (http::stream-remaining-length stream)))

    (http::stream-read-remain stream)
    (is (= 0 (http::stream-remaining-length stream)))))
