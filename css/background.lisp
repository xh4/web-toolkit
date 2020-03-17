(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3

(define-property background-color () ())

(defun background-color (value)
  (if-let ((value (typecase value
                 (string (parse-color value))
                 ((or rgb rgba) value))))
    (make-instance 'background-color :value value)
    (error "Bad background-color value ~A" value)))

(define-property background-image () ())

(defun background-image (&rest values)
  (if (= 1 (cl:length values))
      (let ((value (first values)))
        (if-let ((value (typecase value
                          (string (if (string-equal "none" value)
                                      :none
                                      (let ((parts (split-sequence #\, value)))
                                        (mapcar #'parse-url parts))))
                          (keyword (if (eq :none value) value (error "Bad background-image value ~A" value)))
                          (uri:uri value))))
          (make-instance 'background-image :value value)
          (error "Bad background-image value ~A" value)))
      (loop for value in values
         when (typep value 'uri)
         collect value into images
         else do (error "Bad background-image value ~A" value)
         finally (make-instance 'background-image :value images))))

(define-property background-repeat () ())

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

(defun parse-background-repeat (string)
  (nth-value 1 (parse (.background-repeat) string)))

(defun background-repeat (&rest values)
  (cond
    ((= 1 (cl:length values))
     (let ((value (first values)))
       (when-let ((value (typecase value
                           (keyword (if (member value '(:repeat-x :repeat-y :repeat :space :round :no-repeat))
                                        (case value
                                          (:repeat-x '(:repeat :no-repeat))
                                          (:repeat-y '(:no-repeat :repeat))
                                          (:repeat '(:repeat :repeat))
                                          (:space '(:space :space))
                                          (:round '(:round :round))
                                          (:no-repeat '(:no-repeat :no-repeat)))
                                        (error "Bad background-repeat value ~A" value)))
                           (string (or (parse-background-repeat value)
                                       (error "Bad background-repeat value ~S" value)))
                           (t (error "Bad background-repeat value ~S" value)))))
         (make-instance 'background-repeat :value value))))
    ((= 2 (cl:length values))
     (if (and (member (first values) '(:repeat :space :round :no-repeat))
              (member (second values) '(:repeat :space :round :no-repeat)))
         (make-instance 'background-repeat :value values)
         (error "Bad background-repeat values ~A" values)))))

(define-property background-attachment () ())

(defun background-attachment (&rest values)
  (cond
    ((= 1 (cl:length values))
     (loop for value in values
        when (and (keywordp value)
                  (member value '(:scroll :fixed :local)))
        collect value into vs
        when (and (stringp value)
                  (find value '("scroll" "fixed" "local") :test 'string-equal))
        collect (make-keyword (string-upcase value)) into vs
        finally (return (make-instance 'background-attachment :value vs))))
    (t (error "Bad background-attachment values ~A" values))))

(define-property background-position () ())

;; TODO: validate
(defun parse-background-position (string)
  (let ((parts (split-sequence #\Space string)))
    (let ((parts (loop for part in parts
                    when (find part '("left" "right" "top" "bottom" "center") :test 'equal)
                    collect (make-keyword (string-upcase part))
                    else collect (or (parse-length part)
                                     (parse-percentage part)
                                     (error "Bad background-position value ~S" string)))))
      parts)))

;; TODO: validate
(defun background-position (&rest values)
  (let ((value (if (= 1 (cl:length values))
                   (let ((value (first values)))
                     (typecase value
                       (string (parse-background-position value))
                       (keyword value)
                       (t (error "Bad background-position value ~A" value))))
                   values)))
    (make-instance 'background-position :value value)))

(define-property background-clip () ())

(defun parse-background-clip (string)
  (let ((parts (split-sequence #\Space string)))
    (let ((parts (loop for part in parts
                    when (find part '("border-box" "padding-box" "content-box") :test 'equal)
                    collect (make-keyword (string-upcase part))
                    else do (error "Bad background-position value ~S" string))))
      parts)))

(defun background-clip (&rest values)
  (let ((value (if (= 1 (cl:length values))
                   (let ((value (first values)))
                     (typecase value
                       (string (parse-background-clip value))
                       (keyword (if (member value '(:border-box :padding-box :content-box))
                                    value
                                    (error "Bad background-clip value ~A" value)))
                       (t (error "Bad background-clip value ~A" value))))
                   values)))
    (make-instance 'background-clip :value value)))

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
         when (check-type value shadow)
         collect value into shadows
         finally (return (make-instance 'box-shadow :value shadows)))))
