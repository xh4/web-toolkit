(in-package :http)

(defclass entity (message)
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
  (or (slot-value entity 'status) (entity-status entity) 200))

(defmethod (setf response-status) (status (entity entity))
  (setf (entity-status entity) status))

(defmethod status-code ((entity entity))
  (status-code (response-status entity)))

(defmethod status-keyword ((entity entity))
  (status-keyword (response-status entity)))

(defmethod status-reason-phrase ((entity entity))
  (status-reason-phrase (response-status entity)))

(defmethod response-header ((entity entity))
  (let ((header (copy-header (entity-header entity))))
    (when (find-method #'content-length '() (list (class-of entity)) nil)
      (when-let ((value (content-length entity)))
        (set-header-field header (header-field "Content-Length" value))))
    (when (find-method #'content-type '() (list (class-of entity)) nil)
      (when-let ((value (content-type entity)))
        (set-header-field header (header-field "Content-Type" value))))
    header))

(defmethod (setf response-header) (header (entity entity))
  (setf (entity-header entity) header))

(defmethod response-body ((entity entity))
  (entity-body entity))

(defmethod message-body ((entity entity))
  (response-body entity))

(defmethod (setf response-body) (body (entity entity))
  (setf (entity-body entity) body))

(defmethod find-header-field (name (entity entity))
  (find-header-field name (entity-header entity)))

(defmethod set-header-field ((entity entity) header-field)
  (set-header-field (entity-header entity) header-field))

(defmethod write-response (stream (entity entity))
  (+
   (if-let ((status (response-status entity)))
     (write-status-line stream "HTTP/1.1" (status-code status) (status-reason-phrase status))
     (error "Missing status in response"))
   (write-header stream (response-header entity))
   (write-message-body stream entity)))

(defun copy-header (header)
  (let ((new-header (make-instance 'header)))
    (loop for header-field in (header-fields header)
       do (set-header-field header header-field))
    new-header))
