(in-package :http)

(defclass entity ()
  ((status
    :initarg :status
    :initform nil
    :accessor entity-status)
   (header
    :initarg :header
    :initform (make-instance 'header)
    :accessor entity-header)
   (body
    :initarg :body
    :initform nil
    :accessor entity-body)))

(defgeneric allow (entity))

(defgeneric content-encoding (entity))

(defgeneric content-language (entity))

(defgeneric content-length (entity))

(defgeneric content-location (entity))

(defgeneric content-md5 (entity))

(defgeneric content-range (entity))

(defgeneric content-type (entity))

(defgeneric expires (entity))

(defgeneric last-modified (entity))

(defgeneric entity-body (entity))

(defmethod response-status ((entity entity))
  (entity-status entity))

(defmethod (setf response-status) (status (entity entity))
  (setf (entity-status entity) status))

(defmethod response-header ((entity entity))
  (entity-header entity))

(defmethod (setf response-header) (header (entity entity))
  (setf (entity-header entity) header))

(defmethod response-body ((entity entity))
  (entity-body entity))

(defmethod (setf response-body) (body (entity entity))
  (setf (entity-body entity) body))

(defmethod find-header-field ((entity entity) name)
  (find-header-field (entity-header entity) name))

(defmethod write-response (stream (entity entity))
  (+
   (write-status-line stream "HTTP/1.1"
                      (status-code (response-status entity))
                      (status-reason-phrase (response-status entity)))
   (let ((header (response-header entity)))
     (when (find-method #'content-length '() (list (class-of entity)) nil)
       (when-let ((value (content-length entity)))
         (add-header-field header (header-field "Content-Length" value))))
     (when (find-method #'content-type '() (list (class-of entity)) nil)
       (when-let ((value (content-type entity)))
         (add-header-field header (header-field "Content-Type" value))))
     (write-header stream header))
   (let ((body (response-body entity)))
     (typecase body
       (string (length (write-sequence (babel:string-to-octets body) stream)))
       (vector (length (write-sequence body stream)))))))
