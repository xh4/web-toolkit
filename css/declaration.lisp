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
   (%value
    :initarg :%value
    :initform nil)
   (value
    :initarg :value
    :initform nil
    :accessor declaration-value)
   (important
    :initarg :important
    :initform nil
    :accessor declaration-important))
  (:metaclass declaration-class))

(defmethod initialize-instance :after ((declaration declaration) &key)
  (check-type (slot-value declaration '%value) string)
  (let ((value (slot-value declaration '%value)))
    (setf value (string-trim '(#\Space) value))
    (when-let ((value-types (declaration-class-value (class-of declaration))))
      (when-let ((pos (cl-ppcre:scan "\\s*!important" value)))
        (setf (declaration-important declaration) t)
        (setf value (subseq value 0 pos)))
      (when (or (equal value "inherit")
                (equal value "initial")
                (equal value "unset")
                (equal value "revert"))
        (setf (slot-value declaration 'value) (make-keyword (string-upcase value)))
        (return-from initialize-instance))
      (when (or (string-prefix-p "-ms-" value)
                (string-prefix-p "-webkit-" value)
                (search "calc" value))
        (setf (slot-value declaration 'value) value)
        (return-from initialize-instance))
      (loop for type in value-types
         for v = (cond
                   ((and (symbolp type)
                         (not (keywordp type))
                         (subtypep type 'parser))
                    (multiple-value-bind (rest value match-p)
                        (parse (funcall type) value)
                      (declare (ignore rest))
                      (if match-p
                          value)))
                   ((equal type 'number) (if (find #\. value)
                                             (ignore-errors (parse-float value))
                                             (ignore-errors (parse-integer value))))
                   ((equal type 'integer) (ignore-errors (parse-integer value)))
                   ((integerp type) (when (equal (format nil "~D" type) value) value))
                   ((keywordp type) (when (string-equal
                                           value
                                           (symbol-name type))
                                      type))
                   ((subtypep type 'dimension) (dimension value type)))
         when v do (return (setf (slot-value declaration 'value) v))
         finally (error "Invalid value ~S for declaration ~A" value (type-of declaration))))))

(defmacro define-declaration (declaration-name superclasses slots &rest options)
  (unless (find 'declaration superclasses)
    (appendf superclasses '(declaration)))
  (appendf options '((:metaclass declaration-class)))
  `(progn
     (defclass ,declaration-name ,superclasses ,slots ,@options)
     (defun ,declaration-name (value)
       (make-instance ',declaration-name
                      :name (make-keyword ',declaration-name)
                      :%value value))))

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

(defmacro define-property (name superclasses slots &rest options)
  (unless (find 'property superclasses)
    (appendf superclasses '(property)))
  `(define-declaration ,name ,superclasses ,slots ,@options))

(defmethod print-object ((property property) stream)
  (print-unreadable-object (property stream :type t)
    (with-slots (%value value important) property
      (if value
          (format stream "~A" value)
          (format stream "~S" %value))
      (when important
        (format stream " !")))))

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

(defmacro define-descriptor (name superclasses slots &rest options)
  (unless (find 'descriptor superclasses)
    (appendf superclasses '(descriptor)))
  `(define-declaration ,name ,superclasses ,slots ,@options))

(define-serialize-method ((declaration declaration) stream)
  (let ((name (declaration-name declaration))
        (value (slot-value declaration '%value)))
    (format stream "~(~A~): ~A" name value)))
