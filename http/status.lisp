(in-package :wt.http)

(defclass http-status ()
  ((keyword
    :initarg :keyword
    :reader status-keyword)
   (code
    :initarg :code
    :reader status-code)
   (reason-phrase
    :initarg :reason-phrase
    :reader status-reason-phrase)))

(defmethod print-object ((status http-status) stream)
  (print-unreadable-object (status stream)
    (format stream "~A ~A"
            (status-code status)
            (status-reason-phrase status))))
