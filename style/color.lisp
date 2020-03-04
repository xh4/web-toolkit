(in-package :style)

(define-property color () ())

(defclass type/color () ())

(define-property opacity () ())

(defclass rgb ()
  ((red
    :initarg :red
    :initform 0
    :accessor rgb-red)
   (green
    :initarg :green
    :initform 0
    :accessor rgb-green)
   (blue
    :initarg :blue
    :initform 0
    :accessor rgb-blue)))

(defmethod print-object ((rgb rgb) stream)
  (print-unreadable-object (rgb stream :type t)
    (with-slots (red green blue) rgb
      (format stream "~A ~A ~A" red green blue))))

(defun rgb (red green blue)
  (make-instance 'rgb :red red :green green :blue blue))

(defclass rgba (rgb)
  ((alpha
    :initarg :alpha
    :initform 1
    :accessor rgba-alpha)))

(defmethod print-object ((rgba rgba) stream)
  (print-unreadable-object (rgba stream :type t)
    (with-slots (red green blue alpha) rgba
      (format stream "~A ~A ~A ~A" red green blue alpha))))

(defmethod rgba-red ((rgba rgba))
  (rgb-red rgba))

(defmethod rgba-green ((rgba rgba))
  (rgb-green rgba))

(defmethod rgba-blue ((rgba rgba))
  (rgb-blue rgba))

(defun rgba (red green blue alpha)
  (make-instance 'rgba :red red :green green :blue blue :alpha alpha))
