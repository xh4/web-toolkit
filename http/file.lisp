(in-package :http)

(defclass file ()
  ((pathname
    :initarg :pathname
    :initform nil
    :accessor file-pathname)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t :identity t)
    (format stream "~S" (file-pathname file))))

(defmethod initialize-instance :after ((file file) &key)
  (with-slots (pathname) file
    (check-type pathname pathname)))

(defgeneric file-size (file)
  (:method ((file file))
    (with-slots (pathname) file
      (let ((namestring (namestring pathname)))
        (ignore-errors
          #+sbcl
          (sb-posix:stat-size (sb-posix:stat pathname))
          #+ccl
          (ccl:file-data-size pathname)
          #+(and lispworks unix)
          (sys:file-stat-size (sys:get-file-stat namestring))
          #-(or sbcl ccl (and lispworks unix))
          (with-open-file (stream pathname
                                  :direction :input
                                  :element-type '(unsigned-byte 8))
            (file-length stream)))))))

(defgeneric file-modify-time (file)
  (:method ((file file))
    (file-write-date (file-pathname file))))
