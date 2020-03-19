(in-package :json)

(defclass object ()
  ((pairs
    :initarg :pairs
    :initform nil
    :accessor object-pairs)))

(defun object-form (object)
  (check-type object object)
  (labels ((value-expr (value)
             (typecase value
               (object
                (let ((pairs (slot-value value 'pairs)))
                  (loop for (name . value) in pairs
                     append (list name (value-expr value)) into body
                     finally (return `(object ,@body)))))
               (t value))))
    (value-expr object)))

(defmethod print-object ((object object) stream)
  (let ((*print-case* :downcase))
    (prin1 (object-form object) stream)))

(defun pprint-object (object &optional (stream *standard-output*))
  (check-type object object)
  (let ((*print-case* :downcase)
        (*print-pretty* t)
        (list (object-form object)))
    (pprint-logical-block (nil list :prefix "(" :suffix ")")
      (write (first list))
      (loop for (key value) on (rest list) by #'cddr
         do
           (write-char #\space)
           (pprint-newline :mandatory)
           (write key)
           (write-char #\space)
           (cond
             ((and (listp value)
                   (eq (car value) 'object))
              (pprint-object value stream))
             (t (write value)))))))

(defun object (&rest arguments)
  (when (oddp (length arguments))
    (error "Expect even arguments when initialize object, got ~D" (length arguments)))
  (let* ((object (make-instance 'object)))
    (loop for (name0 value0) on arguments by #'cddr
       for name = (typecase name0
                    (string name0)
                    (t (error "~A of type ~A can't be used as a name for object" name0 (type-of name0))))
       for value = (cond
                     ((eq value0 t) true)
                     ((eq value0 nil) false)
                     ((listp value0) (array value0))
                     ((stringp value0) value0)
                     ((vectorp value0) (array (coerce value0 'list)))
                     (t (typecase value0
                          ((or true false null array object number) value0)
                          (t (format nil "~A" value0)))))
       collect (cons name value) into pairs
       finally (setf (slot-value object 'pairs) pairs))
    object))

(defmacro do-object ((name value object &optional result-form) &body body)
  `(loop :for (,name . ,value) :in (slot-value ,object 'pairs)
      :do
        ,@body
      :finally (return ,result-form)))
