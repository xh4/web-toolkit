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
