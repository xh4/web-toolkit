(in-package :http)

(defclass status ()
  ((keyword
    :initarg :keyword
    :reader status-keyword)
   (code
    :initarg :code
    :reader status-code)
   (reason-phrase
    :initarg :reason-phrase
    :reader status-reason-phrase)))

(defmethod print-object ((status status) stream)
  (print-unreadable-object (status stream)
    (format stream "~A ~A"
            (status-code status)
            (status-reason-phrase status))))
