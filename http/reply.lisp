(in-package :http)

(defmacro reply (&rest forms)
  (with-gensyms (object)
    `(progn
       (setf *response* (or *response* (make-instance 'response)))
       ,@(loop for form in forms
            collect
              `(let ((,object ,form))
                 (reply-object ,object)))
       *response*)))

(defgeneric reply-object (object))

(defmethod reply-object ((status status))
  (setf (response-status *response*) status))

(defmethod reply-object ((header-field header-field))
  (typecase *response*
    (response (add-header-field (response-header *response*) header-field))
    (entity (add-header-field (entity-header *response*) header-field))))

(defmethod reply-object ((header header))
  (loop for header-field in (header-fields header)
     do (typecase *response*
          (response (add-header-field (response-header *response*) header-field))
          (entity (add-header-field (entity-header *response*) header-field)))))

(defmethod reply-object ((response response))
  (setf *response* response))

(defmethod reply-object ((entity entity))
  (setf *response* entity))

(defmethod reply-object ((text string))
  (setf *response* (make-instance 'text-entity
                                  :status (response-status *response*)
                                  :header (response-header *response*)
                                  :body text)))

(defmethod reply-object ((data vector))
  (setf (response-body *response*) data))

(defmethod reply-object ((pathname pathname))
  (setf *response* (make-instance 'file-entity
                                  :status (response-status *response*)
                                  :header (response-header *response*)
                                  :body pathname)))

(defmethod reply-object ((nothing null)))
