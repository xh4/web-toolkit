(in-package :http)

(defgeneric header-field-name (header-field))

(defgeneric (setf header-field-name) (name header-field))

(defgeneric header-field-value (header-field))

(defgeneric (setf header-field-value) (value header-field))

(defgeneric header-field-name-match-p (header-field name))

(defclass header-field ()
  ((name
    :initarg :name
    :initform nil
    :reader header-field-name)
   (value
    :initarg :value
    :initform nil
    :reader header-field-value)))

(defmethod print-object ((header-field header-field) stream)
  (print-unreadable-object (header-field stream :type t)
    (format stream "~A: ~A"
            (header-field-name header-field)
            (header-field-value header-field))))

(defmethod initialize-instance :after ((header-field header-field) &key)
  (setf (header-field-name header-field) (slot-value header-field 'name)
        (header-field-value header-field) (slot-value header-field 'value)))

(defmethod (setf header-field-name) (name header-field)
  (let ((name (typecase name
                (string name)
                (t (header-case (format nil "~A" name))))))
    (setf (slot-value header-field 'name) name)))

(defmethod header-field-name ((header-field null))
  nil)

(defmethod (setf header-field-value) (value header-field)
  (let ((value (typecase value
                 (string value)
                 (t (format nil "~A" value)))))
    (setf (slot-value header-field 'value) value)))

(defmethod header-field-value ((header-field null))
  nil)

(defmethod header-field-name-match-p (header-field name)
  (let ((name (typecase name
                (string name)
                (t (format nil "~A" name)))))
    (string-equal name (header-field-name header-field))))

(defmacro header-field (name value)
  `(make-instance 'header-field :name ,name :value ,value))


(defun read-header-field (stream &key (parse t))
  (let ((line (read-line stream)))
    (unless (emptyp line)
      (if parse
          (parse-header-field line)
          line))))

(defun parse-header-field (line)
  (let ((list (cl-ppcre:split ":\\s+" line :limit 2)))
    (when (= 2 (length list))
      (destructuring-bind (name value) list
        (make-instance 'header-field :name name :value value)))))

(defgeneric write-header-field (stream header-field)
  (:method (stream (header-field header-field))
    (with-slots (name value) header-field
      (let ((line (format nil "~A: ~A" name value)))
        (write-sequence (babel:string-to-octets line) stream)
        (write-sequence +crlf+ stream)
        (+ (length line) (length +crlf+))))))
