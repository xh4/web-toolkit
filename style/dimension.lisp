(in-package :style)

(defclass dimension ()
  ((number
    :initarg :number
    :initform nil
    :accessor dimension-number)
   (unit
    :initarg :unit
    :initform nil
    :accessor dimension-unit)))

(defmacro define-dimension (name (&optional superclass))
  (unless superclass (setf superclass 'dimension))
  `(defclass ,name (,superclass) ()))

(defmacro define-dimension/unit (name (&optional superclass))
  (unless superclass (setf superclass 'dimension))
  `(defclass ,name (,superclass)
     ((unit
       :initform ,(make-keyword (symbol-name name))))))
