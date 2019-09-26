(in-package :http)

(defun add-header-field (header header-field)
  (let ((header-fields (header-fields header)))
    (if-let ((pos (position (header-field-name header-field) header-fields
                            :test (lambda (name header-field)
                                    (header-field-name-match-p header-field
                                                               name)))))
      (setf (nth pos (header-fields header)) header-field)
      (appendf (header-fields header) (list header-field)))))

(defmacro reply (&rest forms)
  (with-gensyms (object)
    `(progn
       ,@(loop for form in forms
            collect
              `(let ((,object ,form))
                 (typecase ,object
                   (status (setf (response-status *response*) ,object))
                   (header-field (add-header-field (response-header *response*)
                                                   ,object))
                   (t (setf (response-body *response*) ,object)))))
       *response*)))
