(in-package :cl-user)

(ql:quickload :uuid)

(defun make-uninterned-symbol ()
  (make-symbol (format nil "~A" (uuid:make-v4-uuid))))

(defclass fluid-class (standard-class) ())

#+sbcl
(defmethod sb-mop:validate-superclass ((class fluid-class)
                                       (superclass standard-class))
  "Any fluid class is also a standard class."
  t)

(defun make-fluid-class ()
  (make-instance 'fluid-class
                 :name #-cmu nil #+cmu (gensym "FLUID")
                 :direct-superclasses nil
                 :direct-slots
                 (loop repeat 100
                    collect `(:name ,(make-uninterned-symbol)))))

;; (loop repeat 1000000 do (make-uninterned-symbol))

;; (loop repeat 10000 do (make-fluid-class))
