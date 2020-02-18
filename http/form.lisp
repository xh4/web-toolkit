(in-package :http)

(defclass form ()
  ((fields
    :initarg :fields
    :initform nil
    :accessor form-fields)))

(defclass form-field ()
  ((name
    :initarg :name
    :initform nil
    :accessor form-field-name)
   (value
    :initarg :value
    :initform nil
    :accessor form-field-value)))

(defmacro form (&rest forms)
  (with-gensyms (form objects)
    `(let ((,form (make-instance 'form)))
       (let ((,objects (flatten (list ,@forms))))
         (process-form-objects ,form ,objects)))))

(defmacro form-field (name value)
  `(make-instance 'form-field :name ,name :value ,value))

(defun process-form-objects (form objects)
  (if objects
      (let ((object (first objects)))
        (typecase object
          (form-field
           (appendf (form-fields form)
                    (list object))
           (process-form-objects form (rest objects)))
          ((or string keyword)
           (when-let ((value (second objects)))
             (appendf (form-fields form)
                      (list (form-field object value))))
           (process-form-objects form (cddr objects)))
          (t (error "Unable to use ~A as a name of header field" object))))
      form))
