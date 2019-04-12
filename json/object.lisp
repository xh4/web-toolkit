(in-package :wt.json)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defclass fluid-class (standard-class) ()))

(clos:finalize-inheritance
 (defclass fluid-object (standard-object)
   ((%slots
     :initarg :%slots
     :initform (make-hash-table :test 'equal)))
   (:metaclass fluid-class)))

(defun pprint-object (list)
  (let ((*print-case* :downcase)
        (*print-pretty* t))
    (pprint-logical-block (nil list :prefix "(" :suffix ")")
      (write (first list))
      (loop for (key value) on (rest list) by 'cddr
         do
           (write-char #\space)
           (pprint-newline :mandatory)
           (write key)
           (write-char #\space)
           (cond
             ((and (listp value)
                   (eq (car value) 'object))
              (pprint-object value))
             (t (write value)))))))

(defmethod print-object ((object fluid-object) stream)
  (labels ((value-expr (value)
             (typecase value
               (fluid-object
                (let ((slots-hash-table (slot-value value '%slots)))
                  (loop for key being the hash-keys of slots-hash-table
                     using (hash-value value)
                     append (list key (value-expr value)) into body
                     finally (return `(object ,@body)))))
               (t value))))
    (pprint-object (value-expr object))))

(defun slot-name-to-object-key (name)
  (typecase name
    (string name)
    (symbol (json::lisp-to-camel-case (symbol-name name)))))

(defmethod clos:make-instance ((class fluid-class) &rest initargs)
  (let ((instance (call-next-method class)))
    (loop for (name value) on initargs by 'cddr
       do (setf (slot-value instance (slot-name-to-object-key name)) value))
    instance))

(defmethod clos:slot-value-using-class ((class fluid-class) instance slot-name)
  (if (eq slot-name '%slots)
      (call-next-method class instance '%slots)
      (let ((slots-hash-table (slot-value instance '%slots)))
        (gethash (slot-name-to-object-key slot-name) slots-hash-table))))

(defmethod (setf clos:slot-value-using-class) (new-value (class fluid-class) instance slot-name)
  (let ((slots-hash-table (slot-value instance '%slots)))
    (setf (gethash (slot-name-to-object-key slot-name) slots-hash-table)
          new-value)))

(defmethod clos:slot-boundp-using-class ((class fluid-class) instance slot-name)
  (if (eq slot-name '%slots)
      t
      (let ((slots-hash-table (slot-value instance '%slots)))
        (multiple-value-bind (value present-p)
            (gethash (slot-name-to-object-key slot-name) slots-hash-table)
          (declare (ignore value))
          present-p))))

(defmethod clos:slot-makunbound-using-class ((class fluid-class) instance slot-name)
  (let ((slots-hash-table (slot-value instance '%slots)))
    (remhash (slot-name-to-object-key slot-name) slots-hash-table)))

(defun object (&rest initargs)
  (apply 'make-instance 'fluid-object initargs))

(defun alist-object (alist)
  (let ((initargs (loop for (name . value) in alist
                     append (list name value))))
    (apply 'make-instance 'fluid-object initargs)))
