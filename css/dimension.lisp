(in-package :css)

(defclass dimension ()
  ((number
    :initarg :number
    :initform nil
    :accessor dimension-number)
   (unit
    :initarg :unit
    :initform nil
    :accessor dimension-unit)))

(defmethod print-object ((dimension dimension) stream)
  (print-unreadable-object (dimension stream :type t)
    (format stream "~A" (dimension-number dimension))))

(defmethod initialize-instance :after ((dimension dimension) &key)
  (unless (dimension-number dimension)
    (error "Missing number when initialize dimension ~A" (type-of dimension)))
  (check-type (dimension-number dimension) number))

(defmacro define-dimension (name (&optional superclass))
  (unless superclass (setf superclass 'dimension))
  `(defclass ,name (,superclass) ()))

(defvar *dimension-units* '())

(defmacro define-dimension-unit (name (&optional superclass))
  (unless superclass (setf superclass 'dimension))
  `(progn
     (defclass ,name (,superclass)
       ((unit
         :initform ,(make-keyword (symbol-name name)))))
     (defun ,name (number)
       (make-instance ',name :number number))
     (pushnew ',name *dimension-units*)))

(defun dimension (value &optional type)
  (typecase value
    (number (when type
              (loop for unit in *dimension-units*
                 when (string-equal (symbol-name type) (symbol-name unit))
                 do (return (funcall unit value)))))
    (dimension (if type (when (typep value type) value) value))
    (string (when-let* ((groups (coerce
                                 (nth-value 1 (cl-ppcre:scan-to-strings "([0-9.]+)([A-Za-z]+)" value))
                                 'list))
                        ;; FIXME: parse number
                        (number (parse-integer (first groups) :junk-allowed t))
                        (suffix (second groups)))
              (loop for unit in *dimension-units*
                 when (and (string-equal suffix (symbol-name unit))
                           (or (null type)
                               (subtypep unit type)))
                 do (return (funcall unit number)))))))
