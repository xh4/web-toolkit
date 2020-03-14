(in-package :json)

(defclass object ()
  ((pairs
    :initarg :pairs
    :initform nil)))

(defun pprint-object (list &optional (stream *standard-output*))
  (let ((*print-case* :downcase)
        (*print-pretty* t))
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

(defmethod print-object ((object object) stream)
  (labels ((value-expr (value)
             (typecase value
               (object
                (let ((pairs (slot-value value 'pairs)))
                  (loop for (name . value) in pairs
                     append (list name (value-expr value)) into body
                     finally (return `(object ,@body)))))
               (t value))))
    (let ((*print-case* :downcase))
      (prin1 (value-expr object) stream))))

(defun lisp-name-to-object-key (name)
  (typecase name
    (string name)
    (symbol (lisp-to-camel-case (symbol-name name)))))

(defun object (&rest arguments)
  (when (oddp (length arguments))
    (error "Expect even arguments when initialize object, got ~D" (length arguments)))
  (let* ((object (make-instance 'object)))
    (loop for (name0 value0) on arguments by #'cddr
       for name = (typecase name0
                    (string name0)
                    (symbol (lisp-name-to-object-key name0))
                    (t (error "Value ~A of type ~A can't be a name of object" name0 (type-of name0))))
       for value = (typecase value0
                     ((or string symbol number
                          sequence object array null) value0)
                     (t (format nil "~A" value0)))
       collect (cons name value) into pairs
       finally (setf (slot-value object 'pairs) pairs))
    object))

(defmacro do-object ((name value object &optional result-form) &body body)
  `(loop :for (,name . ,value) :in (slot-value ,object 'pairs)
      :do
        ,@body
      :finally (return ,result-form)))
