(in-package :http-test)

(defmacro with-input-from-lines ((var lines &key (line-break http::+crlf+)) &body body)
  `(let* ((line-break (typecase ,line-break
                        (vector ,line-break)
                        (string (babel:string-to-octets ,line-break))
                        (character (babel:string-to-octets (string ,line-break)))))
          (line-octets (mapcar 'babel:string-to-octets ,lines))
          (all-octets (loop with all-octets = #()
                         for octets in line-octets
                         do (setf all-octets (concatenate 'vector all-octets octets)
                                  all-octets (concatenate 'vector all-octets line-break))
                         finally (return all-octets))))
     (babel-streams:with-input-from-sequence (,var all-octets)
       ,@body)))

(defun stream-length-p (n stream)
  (let ((content (alexandria::read-stream-content-into-byte-vector stream)))
    (= n (length content))))

(defun stream-empty-p (stream)
  (stream-length-p 0 stream))

(defclass test-socket ()
  ((open-p
    :initform t
    :accessor socket-open-p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar test-listener (listener :port 4004)))

(define-server test-server ()
  ()
  (:listener test-listener))

(defmacro with-request-in-stream ((stream request) &body body)
  `(let ((data (babel-streams:with-output-to-sequence (stream)
                 (loop for request in (ensure-list ,request)
                    do (http::write-request stream request)))))
     (let ((,stream (babel-streams:make-in-memory-input-stream data)))
       ,@body)))

(defmacro with-output-to-string ((stream) &body body)
  `(babel:octets-to-string
    (babel-streams:with-output-to-sequence (,stream)
      ,@body)))

(defmacro with-read-header-field ((var line) &body body)
  `(with-input-from-lines (stream '(,line))
     (let ((,var (http::read-header-field stream)))
       (is-true (stream-empty-p stream))
       ,@body)))

(defmacro with-read-header ((var lines) &body body)
  `(with-input-from-lines (stream ,lines)
     (let ((,var (http::read-header stream)))
       ,@body)))
