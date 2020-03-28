(in-package :css)

(defclass pseudo ()
  ((classes
    :initarg :classes
    :initform nil
    :accessor pseudo-classes)
   (properties
    :initarg :properties
    :initform nil
    :accessor pseudo-properties)))

(defmethod print-object ((pseudo pseudo) stream)
  (print-unreadable-object (pseudo stream :type t :identity t)
    (with-slots (classes properties) pseudo
      (format stream "~A {~A}" classes (cl:length properties)))))

(defun pseudo (classes &rest objects)
  (setf classes (ensure-list classes))
  (loop for object in (flatten objects)
     when (typep object 'property)
     collect object into properties
     finally (return (make-instance 'pseudo
                                    :classes classes
                                    :properties properties))))

(defmacro define-pseudo (name)
  `(defun ,name (&rest objects)
     (pseudo ,(make-keyword name) objects)))

(define-pseudo hover)

(define-pseudo active)

(define-pseudo focus)

(define-pseudo enabled)

(define-pseudo disabled)

(define-pseudo link)

(define-pseudo visited)

(define-pseudo valid)

(define-pseudo invalid)

(define-pseudo empty)
