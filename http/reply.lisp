(in-package :http)

(defmacro reply (&rest forms)
  (with-gensyms (object)
    `(let ((*response* (or *response* (make-instance 'response))))
       ,@(loop for form in forms
            collect
              `(let ((,object ,form))
                 (reply-object ,object)))
       *response*)))

(defgeneric reply-object (object))

(defmethod reply-object ((status status))
  (setf (response-status *response*) status))

(defmethod reply-object ((header-field header-field))
  (set-header-field *response* header-field))

(defmethod reply-object ((header header))
  (loop for header-field in (header-fields header)
     do (set-header-field *response* header-field)))

(defmethod reply-object ((response response))
  (setf *response* response))

(defmethod reply-object ((entity entity))
  (setf *response* entity))

(defmethod reply-object ((text string))
  (setf *response* (make-text-entity text
                                     :status (response-status *response*)
                                     :header (response-header *response*))))

(defmethod reply-object ((data vector))
  (setf (response-body *response*) data))

(defmethod reply-object ((pathname pathname))
  (let ((entity (if (directory-pathname-p pathname)
                    (make-directory-entity pathname
                                           :status (response-status *response*)
                                           :header (response-header *response*))
                    (make-file-entity pathname
                                      :status (response-status *response*)
                                      :header (response-header *response*)))))
    (setf *response* entity)))

(defmethod reply-object ((nothing null)))

(defmethod reply-object ((object json:object))
  (setf *response* (make-json-entity object
                                     :status (response-status *response*)
                                     :header (response-header *response*))))

(defmethod reply-object ((document html:document))
  (setf *response* (make-html-entity document
                                     :status (response-status *response*)
                                     :header (response-header *response*))))

(defmethod reply-object ((element html:element))
  (setf *response* (make-html-entity element
                                     :status (response-status *response*)
                                     :header (response-header *response*))))
