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

(define-parser .dimension ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        ;; FIXME: more strict rule
        (parse (.seq (.some/s (.or (.digit) (.s ".") (.s "-")))
                     (.maybe (.some/s (.alpha))))
               input)
      (if match-p
          (let ((n (ignore-errors (parse-number (first value))))
                (u (second value)))
            (if n
                (if u
                    (loop for unit in *dimension-units*
                       when (string-equal u (symbol-name unit))
                       do (return (values rest (funcall unit n) t))
                       finally (return (values input nil nil)))
                    (values rest (make-instance 'length :number n) t))
                (values input nil nil)))
          (values input nil nil)))))

(define-serialize-method ((dimension dimension) stream)
  (let ((number (dimension-number dimension))
        (unit (dimension-unit dimension)))
    (format stream "~A~(~A~)" number (symbol-name unit))))
