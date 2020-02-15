(in-package :http)

(defclass directory-entity (entity)
  ((directory
    :initarg :directory
    :initform nil
    :accessor entity-directory)
   (html
    :initarg :html
    :initform nil
    :accessor entity-html)
   (html-octets
    :initarg :html-octets
    :initform nil
    :accessor entity-html-octets)))

(defmethod initialize-instance :after ((entity directory-entity) &key)
  (check-type (entity-body entity) pathname)
  (let ((pathname (entity-body entity)))
    (unless (directory-pathname-p pathname)
      (error "Pathname ~S should denote a directory" pathname))
    (let ((directory (make-instance 'directory :pathname pathname)))
      (setf (entity-directory entity) directory))
    (entity-html entity)))

(defmethod print-object ((entity directory-entity) stream)
  (print-unreadable-object (entity stream :type t :identity t)
    (let ((pathname (entity-body entity)))
      (format stream "~S" pathname))))

(defmethod last-modified ((entity directory-entity))
  (let ((directory (entity-directory entity)))
    (let ((modify-time (directory-modify-time directory)))
      (rfc-1123-date modify-time))))

(defmethod entity-status ((entity directory-entity))
  (let ((pathname (entity-body entity)))
    (if (probe-file pathname)
        200
        404)))

(defmethod content-type ((entity directory-entity))
  "text/html; charset=UTF-8")

(defmethod entity-html ((entity directory-entity))
  (unless (slot-value entity 'html)
    (let ((html (directory-entity-html entity)))
      (setf (slot-value entity 'html) html
            (slot-value entity 'html-octets) (babel:string-to-octets
                                              (html:serialize html))))))

(defun directory-entity-html (entity)
  (html:document
   (html:html
    (html:head
     (html:title
      ))
    (html:body
     (html:ul
      (loop for content in (directory-content (entity-directory entity))
         for pathname = (typecase content
                          (file (file-pathname content))
                          (directory (directory-pathname content)))
         collect
           (html:li (format nil "~S" pathname))))))))

(defmethod content-length ((entity directory-entity))
  (length (entity-html-octets entity)))

(defmethod response-body ((entity directory-entity))
  (entity-html-octets entity))
