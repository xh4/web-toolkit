(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3

(define-property background-color ()
  ()
  (:value color))

(define-property box-shadow () ())

(defclass shadow ()
  ((horizontal-offset
    :initarg :horizontal-offset
    :initform nil
    :accessor shadow-horizontal-offset)
   (vertical-offset
    :initarg :vertical-offset
    :initform nil
    :accessor shadow-vertical-offset)
   (blur-radius
    :initarg :blur-radius
    :initform nil
    :accessor shadow-blur-radius)
   (spread-distance
    :initarg :spread-distance
    :initform nil
    :accessor shadow-spread-distance)
   (color
    :initarg :color
    :initform nil
    :accessor shadow-color)
   (inset
    :initarg :inset
    :initform nil
    :accessor shadow-inset)))

;; TODO: shadow constructor
(defun shadow (&rest values)
  )

(define-parser .shadow ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.anyorder (.seq (.maybe (.some (.whitespace)))
                                (.length)
                                (.maybe (.some (.whitespace)))
                                (.length)
                                (.maybe (.some (.whitespace)))
                                (.maybe (.length))
                                (.maybe (.some (.whitespace)))
                                (.maybe (.length))
                                (.maybe (.some (.whitespace))))
                          (.seq (.maybe (.some (.whitespace)))
                                (.color)
                                (.maybe (.some (.whitespace))))
                          (.seq (.maybe (.some (.whitespace)))
                                (.s "inset")
                                (.maybe (.some (.whitespace)))))
               input)
      (if match-p
          (loop with lengths = '()
             with color = nil
             with inset = nil
             for value in (flatten value)
             do (typecase value
                  ((or rgb rgba) (setf color value))
                  (length (appendf lengths `(,value)))
                  (string (setf inset t)))
             finally (return (values
                              rest
                              (make-instance 'shadow
                                             :horizontal-offset (first lengths)
                                             :vertical-offset (second lengths)
                                             :blur-radius (third lengths)
                                             :spread-distance (fourth lengths)
                                             :color color
                                             :inset inset)
                              t)))
          (values input nil nil)))))

(define-parser .box-shadow ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.shadow)
                          (.any (.seq (.maybe (.some (.whitespace)))
                                      (.s ",")
                                      (.maybe (.some (.whitespace)))
                                      (.shadow))))
                    (.shadow))
               input)
      (if match-p
          (values rest
                  (loop for v in (flatten value)
                     when (typep v 'shadow)
                     collect v)
                  t)
          (values input nil nil)))))

(defun parse-box-shadow (string)
  (nth-value 1 (parse (.box-shadow) string)))

(defun box-shadow (&rest values)
  (if (= 1 (cl:length values))
      (let ((value (first values)))
        (let ((value (typecase value
                       (keyword (if (eq value :none) value (error "Bad box-shadow value ~A" value)))
                       (string (or (parse-box-shadow value) (error "Bad box-shadow value ~S" value)))
                       (shadow (ensure-list value)))))
          (make-instance 'box-shadow :value value)))
      (loop for value in value
         when (check-type value 'shadow)
         collect value into shadows
         finally (return (make-instance 'box-shadow :value shadows)))))
