(in-package :websocket)

(defun octets-to-string (vector &key (encoding :utf-8))
  (check-type vector vector)
  (if (emptyp vector)
      ""
      (progn
        (when (and (typep vector 'vector)
                   (not (typep vector '(vector (unsigned-byte 8)))))
          (setf vector (coerce vector '(vector (unsigned-byte 8)))))
        (babel:octets-to-string vector :encoding encoding))))

(defun string-to-octets (string &key (encoding :utf-8))
  (babel:string-to-octets string :encoding encoding))
