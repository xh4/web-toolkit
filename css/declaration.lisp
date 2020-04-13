(in-package :css)

(defclass declaration-class (standard-class)
  ((value
    :initarg :value
    :initform nil
    :accessor declaration-class-value)))

(defmethod validate-superclass ((class declaration-class) (super-class standard-class))
  t)

(defclass declaration ()
  ((name
    :initarg :name
    :initform nil
    :accessor declaration-name)
   (value
    :initarg :value
    :initform nil
    :accessor declaration-value)
   (important
    :initarg :important
    :initform nil
    :accessor declaration-important))
  (:metaclass declaration-class))

(defmethod print-object ((declaration declaration) stream)
  (print-unreadable-object (declaration stream :type t :identity t)
    (format stream "NAME: ~S VALUE: ~S"
            (declaration-name declaration)
            (declaration-value declaration))
    (when (declaration-important declaration)
      (format stream " IMPORTANT: T"))))

(defmethod initialize-instance :after ((declaration declaration) &key)
;;;   (let ((value (slot-value declaration '%value)))
;;;     (setf value (string-trim '(#\Space) value))
;;;     (when-let ((value-types (declaration-class-value (class-of declaration))))
;;;       (when-let ((pos (cl-ppcre:scan "\\s*!important" value)))
;;;         (setf (declaration-important declaration) t)
;;;         (setf value (subseq value 0 pos)))
;;;       (when (or (equal value "inherit")
;;;                 (equal value "initial")
;;;                 (equal value "unset")
;;;                 (equal value "revert"))
;;;         (setf (slot-value declaration 'value) (make-keyword (string-upcase value)))
;;;         (return-from initialize-instance))
;;;       (when (or (string-prefix-p "-ms-" value)
;;;                 (string-prefix-p "-webkit-" value)
;;;                 (search "calc" value))
;;;         (setf (slot-value declaration 'value) value)
;;;         (return-from initialize-instance))
;;;       (loop for type in value-types
;;;          for v = (cond
;;;                    ((and (symbolp type)
;;;                          (not (keywordp type))
;;;                          (subtypep type 'utility:parser))
;;;                     (multiple-value-bind (rest value match-p)
;;;                         (parse (funcall type) value)
;;;                       (declare (ignore rest))
;;;                       (if match-p
;;;                           value)))
;;;                    ((equal type 'number) (ignore-errors (parse-number value)))
;;;                    ((equal type 'integer) (ignore-errors (parse-integer value)))
;;;                    ((integerp type) (when (equal (format nil "~D" type) value) value))
;;;                    ((keywordp type) (when (string-equal
;;;                                            value
;;;                                            (symbol-name type))
;;;                                       type)))
;;;          when v do (return (setf (slot-value declaration 'value) v))
;;;          finally (error "Invalid value ~S for declaration ~A" value (type-of declaration)))))
  )

(defmacro define-declaration (declaration-name superclasses slots &rest options)
  (unless (find 'declaration superclasses)
    (appendf superclasses '(declaration)))
  (appendf options '((:metaclass declaration-class)))
  `(defclass ,declaration-name ,superclasses ,slots ,@options))

(define-declaration property () ())

(defgeneric property-name (property)
  (:method ((property property))
    (declaration-name property)))

(defgeneric property-value (property)
  (:method ((property property))
    (declaration-value property)))

(defgeneric property-important (property)
  (:method ((property property))
    (declaration-important property)))

(defmethod initialize-instance :after ((property property) &key)
  (check-type (property-name property) string)
  (check-type (property-value property) string))

(defmacro define-property (property-name superclasses slots &rest options)
  (unless (find 'property superclasses)
    (appendf superclasses '(property)))
  `(progn
     (define-declaration ,property-name ,superclasses ,slots ,@options)
     (defun ,property-name (value)
       (make-instance ',property-name
                      :name (format nil "~(~A~)" ',property-name)
                      :value value))))

(defun property (name value)
  (make-instance 'property :name name :value value))

(defmethod print-object ((property property) stream)
  (print-unreadable-object (property stream)
    (let ((name (property-name property))
          (value (property-value property))
          (important (property-important property)))
      (format stream "~:@(~A~) ~S" name value)
      (when important
        (format stream " !important")))))

(define-serialize-method (property stream)
  (let ((name (property-name property))
        (value (property-value property))
        (important (property-important property)))
    (format stream "~(~A~): ~A" name value)
    (when important
      (format stream " !important"))))

(define-declaration descriptor () ())

(defgeneric descriptor-name (descriptor)
  (:method ((descriptor descriptor))
    (declaration-name descriptor)))

(defgeneric descriptor-value (descriptor)
  (:method ((descriptor descriptor))
    (declaration-value descriptor)))

(defgeneric descriptor-important (descriptor)
  (:method ((descriptor descriptor))
    (declaration-important descriptor)))

(defmacro define-descriptor (descriptor-name superclasses slots &rest options)
  (unless (find 'descriptor superclasses)
    (appendf superclasses '(descriptor)))
  `(define-declaration ,descriptor-name ,superclasses ,slots ,@options))
