(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3

(define-property background-color ()
  ()
  (:value .color))

(define-property background-image ()
  ()
  (:value .bg-image))

;; TODO: .bg-image
(define-parser .bg-image ()
  (lambda (input)
    (value input nil nil)))

(define-property background-repeat ()
  ()
  (:value .background-repeat))

(define-parser .background-repeat ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.or (.s "repeat")
                               (.s "space")
                               (.s "round")
                               (.s "no-repeat"))
                          (.some (.whitespace))
                          (.or (.s "repeat")
                               (.s "space")
                               (.s "round")
                               (.s "no-repeat")))
                    (.s "repeat-x")
                    (.s "repeat-y")
                    (.s "repeat")
                    (.s "space")
                    (.s "round")
                    (.s "no-repeat"))
               input)
      (if match-p
          (let ((value (if (stringp value)
                           (case (make-keyword (string-upcase value))
                             (:repeat-x '(:repeat :no-repeat))
                             (:repeat-y '(:no-repeat :repeat))
                             (:repeat '(:repeat :repeat))
                             (:space '(:space :space))
                             (:round '(:round :round))
                             (:no-repeat '(:no-repeat :no-repeat)))
                           `(,(make-keyword (string-upcase (first value)))
                              ,(make-keyword (string-upcase (third value)))))))
            (values rest value t))
          (values input nil nil)))))

;; (background-repeat "repeat no-repeat")

(define-property background-attachment ()
  ()
  (:value .background-attachment))

(define-parser .background-attachment ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.some (.seq (.maybe (.some (.whitespace)))
                            (.or (.s "scroll") (.s "fixed") (.s "local"))
                            (.maybe (.some (.whitespace)))))
               input)
      (if match-p
          (values rest
                  (loop for v in (flatten value)
                     when (stringp v)
                     collect (make-keyword (string-upcase v)))
                  t)
          (values input nil nil)))))

;; (background-attachment "scroll fixed")

(define-property background-position ()
  ()
  (:value .bg-position))

;; TODO: .bg-position
(define-parser .bg-position ()
  (lambda (input)
    (values input nil nil)))

(define-property background-clip ()
  ()
  (:value .background-clip))

(define-parser .background-clip ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.some (.seq (.maybe (.some (.whitespace)))
                            (.or (.s "border-box") (.s "padding-box") (.s "content-box"))
                            (.maybe (.some (.whitespace)))))
               input)
      (if match-p
          (values rest
                  (loop for v in (flatten value)
                     when (stringp v)
                     collect (make-keyword (string-upcase v)))
                  t)
          (values input nil nil)))))

(define-property background-origin ()
  ()
  (:value .background-origin))

;; same as .background-clip
(define-parser .background-origin ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.some (.seq (.maybe (.some (.whitespace)))
                            (.or (.s "border-box") (.s "padding-box") (.s "content-box"))
                            (.maybe (.some (.whitespace)))))
               input)
      (if match-p
          (values rest
                  (loop for v in (flatten value)
                     when (stringp v)
                     collect (make-keyword (string-upcase v)))
                  t)
          (values input nil nil)))))

(define-property background-size ()
  ()
  (:value .background-size))

(define-parser .background-size ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.oneof (.or (.seq (.or (.length) (.percentage) (.s "auto"))
                                  (.some (.whitespace))
                                  (.or (.length) (.percentage) (.s "auto")))
                            (.or (.length) (.percentage) (.s "auto")))
                       (.s "cover")
                       (.s "contain"))
               input)
      (if match-p
          (values rest
                  (loop for v in (flatten value)
                     when (stringp v)
                     collect (make-keyword (string-upcase v))
                     when (or (typep v 'length)
                              (typep v 'percentage))
                     collect v)
                  t)
          (values input nil nil)))))

;; (background-size "100% 100%")
;; (background-size "50% auto")

;; TODO: background
(define-property background ()
  ())

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
                                (.some (.whitespace))
                                (.length)
                                (.some (.whitespace))
                                (.maybe (.length))
                                (.some (.whitespace))
                                (.maybe (.length))
                                (.maybe (.some (.whitespace))))
                          (.seq (.maybe (.some (.whitespace)))
                                (.color)
                                (.maybe (.some (.whitespace))))
                          (.seq (.maybe (.some (.whitespace)))
                                (.maybe (.s "inset"))
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

;; (parse (.shadow) "64px 64px 12px 40px rgba(0,0,0,0.4)")
;; (parse (.shadow) "inset 64px 64px 12px 40px rgba(0,0,0,0.4)")

(define-property box-shadow ()
  ()
  (:value .box-shadow))

(define-parser .box-shadow ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.s "none")
                    (.seq (.shadow)
                          (.any (.seq (.maybe (.some (.whitespace)))
                                      (.s ",")
                                      (.maybe (.some (.whitespace)))
                                      (.shadow))))
                    (.shadow))
               input)
      (if match-p
          (values rest
                  (if (and (stringp value) (string-equal value "none"))
                      :none
                      (loop for v in (flatten value)
                         when (typep v 'shadow)
                         collect v))
                  t)
          (values input nil nil)))))

;; (box-shadow "64px 64px 12px 40px rgba(0,0,0,0.4), 12px 12px 0px 8px rgba(0,0,0,0.4) inset")
