(in-package :json)

(defclass object ()
  ((pairs
    :initarg :pairs
    :initform (make-hash-table :test 'equal))))

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
                (let ((pairs-hash-table (slot-value value 'pairs)))
                  (loop for key being the hash-keys of pairs-hash-table
                     using (hash-value value)
                     append (list key (value-expr value)) into body
                     finally (return `(object ,@body)))))
               (t value))))
    (if *print-pretty*
      (pprint-object (value-expr object) stream)
      (let ((*print-case* :downcase))
        (print (value-expr object) stream)))))

(defun lisp-name-to-object-key (name)
  (typecase name
    (string name)
    (symbol (cl-json::lisp-to-camel-case (symbol-name name)))))

(defun object (&rest arguments)
  (when (oddp (length arguments))
    (error "Expect even arguments when initialize object, got ~D" (length arguments)))
  (let* ((object (make-instance 'object))
         (pairs (slot-value object 'pairs)))
    (loop for (name0 value0) on arguments by 'cddr
       for name = (typecase name0
                    (string name0)
                    (symbol (lisp-name-to-object-key name0))
                    (t (error "Value ~A of type ~A can't be a name of object" name0 (type-of name0))))
       for value = (typecase value0
                     ((or string symbol number
                          sequence object) value0)
                     (t (format nil "~A" value0)))
       do
         (setf (gethash name pairs) value))
    object))

(defun alist-object (alist)
  (let ((values (loop for (name . value) in alist
                     append (list name value))))
    (apply 'object values)))

(defun plist-object (plist)
  (let ((values (loop for (name value) on plist by #'cddr
                   append (list name value))))
    (apply 'object values)))
