(in-package :http)

(defclass response ()
  ((status
    :initarg :status
    :initform nil
    :accessor response-status)
   (header
    :initarg :header
    :initform (make-instance 'header)
    :accessor response-header)
   (body
    :initarg :body
    :initform nil
    :accessor response-body)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (let ((*print-pretty* t))
      (pprint-logical-block (stream nil)
        (pprint-indent :block -8 stream)
        (pprint-newline :mandatory stream)
        (format stream "~A" (response-status response))
        (pprint-indent :block -8 stream)
        (pprint-newline :mandatory stream)
        (format stream "~A" (response-header response))
        (pprint-indent :block -8 stream)
        (pprint-newline :mandatory stream)
        (format stream "~A" (response-body response))))))

(defvar *response* nil)

(defgeneric response-status (response))

(defgeneric (setf response-status) (value response))

(defgeneric response-header (response))

(defgeneric (setf response-header) (value response))

(defgeneric response-body (response))

(defgeneric (setf response-body) (value response))

(defmethod (setf response-status) ((value integer) response)
  (let ((status (gethash value
                         *status-code-mapping-table*)))
    (setf (response-status response) status)))

(defmethod (setf response-status) ((value symbol) response)
  (let ((status (gethash (make-keyword value)
                         *status-keyword-mapping-table*)))
    (setf (response-status response) status)))

(defmethod (setf response-status) ((value status) response)
  (setf (slot-value response 'status) value))

(defmethod header-fields ((response response))
  (let ((header (response-header response)))
    (header-fields header)))

(defmethod (setf header-fields) (value (response response))
  (let ((header (response-header response)))
    (setf (header-fields header) value)))

(defmethod find-header-field ((response response) name)
  (find-header-field (response-header response) name))
