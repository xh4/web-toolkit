(in-package :http-test)

(defmacro with-input-from-lines ((var lines) &body body)
  `(let* ((line-octets (mapcar 'babel:string-to-octets ,lines))
          (all-octets (loop with all-octets = #()
                         for octets in line-octets
                         if (emptyp all-octets)
                         do (setf all-octets octets)
                         else
                         do (setf all-octets (concatenate 'vector all-octets http::+crlf+)
                                  all-octets (concatenate 'vector all-octets octets))
                         finally (return all-octets))))
     (babel-streams:with-input-from-sequence (,var all-octets)
       ,@body)))
