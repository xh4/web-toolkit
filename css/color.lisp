(in-package :css)

;; https://www.w3.org/TR/css-color-3

(define-property color () ())

(define-property opacity () ())

(define-parser .alphavalue ()
  (lambda (input)
    (multiple-value-bind (value n)
        (parse-float (subseq
                      (maxpc.input.vector::index-vector-vector input)
                      (maxpc.input.index::index-position input))
                     :junk-allowed t)
      (if (and value (<= 0 value 1))
          (values (loop with rest = input
                     repeat n
                     do (setf rest (maxpc::input-rest rest))
                     finally (return rest))
                  value
                  t)
          (values input nil nil)))))

(defun parse-alpha (string)
  (nth-value 1 (parse (.alphavalue) string)))

(define-parser .color-hex ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.s "#") (.hexdig) (.hexdig) (.hexdig)
                          (.hexdig) (.hexdig) (.hexdig))
                    (.seq (.s "#") (.hexdig) (.hexdig) (.hexdig) (.end)))
               input)
      (if match-p
          (case (cl:length value)
            (5 (let ((r (format nil "~C~C" (nth 1 value) (nth 1 value)))
                     (g (format nil "~C~C" (nth 2 value) (nth 2 value)))
                     (b (format nil "~C~C" (nth 3 value) (nth 3 value))))
                 (values rest (rgb (parse-integer r :radix 16)
                                   (parse-integer g :radix 16)
                                   (parse-integer b :radix 16))
                         t)))
            (7 (let ((r (format nil "~C~C" (nth 1 value) (nth 2 value)))
                     (g (format nil "~C~C" (nth 3 value) (nth 4 value)))
                     (b (format nil "~C~C" (nth 5 value) (nth 6 value))))
                 (values rest (rgb (parse-integer r :radix 16)
                                   (parse-integer g :radix 16)
                                   (parse-integer b :radix 16))
                         t))))
          (values input nil nil)))))

(defun parse-color-hex (string)
  (nth-value 1 (parse (.color-hex) string)))

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
  (check-type red integer)
  (assert (<= 0 red 255))
  (check-type green integer)
  (assert (<= 0 green 255))
  (check-type blue integer)
  (assert (<= 0 blue 255))
  (make-instance 'rgb :red red :green green :blue blue))

(define-parser .color-number ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.some/s (.digit)) input)
      (if match-p
          (let ((n (parse-integer value)))
            (if (<= 0 n 255)
                (values rest n t)
                (values input nil nil)))
          (values input nil nil)))))

;; TODO: support rgb(110%, 0%, 0%)
(define-parser .rgb ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.s "rgb")
                          (.maybe (.some (.whitespace)))
                          (.s "(")
                          (.maybe (.some (.whitespace)))
                          (.color-number)
                          (.maybe (.some (.whitespace)))
                          (.s ",")
                          (.maybe (.some (.whitespace)))
                          (.color-number)
                          (.maybe (.some (.whitespace)))
                          (.s ",")
                          (.maybe (.some (.whitespace)))
                          (.color-number)
                          (.maybe (.some (.whitespace)))
                          (.s ")")))
               input)
      (if match-p
          (let ((r (nth 4 value))
                (g (nth 8 value))
                (b (nth 12 value)))
            (values rest (rgb r g b) t))
          (values input nil nil)))))

(defun parse-rgb (string)
  (nth-value 1 (parse (.rgb) string)))

(defclass rgba (rgb)
  ((alpha
    :initarg :alpha
    :initform 1
    :accessor rgba-alpha)))

(defmethod print-object ((rgba rgba) stream)
  (print-unreadable-object (rgba stream :type t)
    (with-slots (red green blue alpha) rgba
      (format stream "~A ~A ~A ~A" red green blue alpha))))

(defmethod rgba-red ((rgb rgb))
  (rgb-red rgb))

(defmethod rgba-red ((rgba rgba))
  (rgb-red rgba))

(defmethod rgba-green ((rgb rgb))
  (rgb-green rgb))

(defmethod rgba-green ((rgba rgba))
  (rgb-green rgba))

(defmethod rgba-blue ((rgb rgb))
  (rgb-blue rgb))

(defmethod rgba-blue ((rgba rgba))
  (rgb-blue rgba))

(defmethod rgba-alpha ((rgb rgb))
  1)

(defun rgba (red green blue alpha)
  (check-type red integer)
  (assert (<= 0 red 255))
  (check-type green integer)
  (assert (<= 0 green 255))
  (check-type blue integer)
  (assert (<= 0 blue 255))
  (check-type alpha number)
  (assert (<= 0 alpha 1))
  (make-instance 'rgba :red red :green green :blue blue :alpha alpha))

;; TODO: support rgba(100%,0%,0%,1)
(define-parser .rgba ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.s "rgba")
                          (.maybe (.some (.whitespace)))
                          (.s "(")
                          (.maybe (.some (.whitespace)))
                          (.color-number)
                          (.maybe (.some (.whitespace)))
                          (.s ",")
                          (.maybe (.some (.whitespace)))
                          (.color-number)
                          (.maybe (.some (.whitespace)))
                          (.s ",")
                          (.maybe (.some (.whitespace)))
                          (.color-number)
                          (.maybe (.some (.whitespace)))
                          (.s ",")
                          (.maybe (.some (.whitespace)))
                          (.alphavalue)
                          (.maybe (.some (.whitespace)))
                          (.s ")")))
               input)
      (if match-p
          (let ((r (nth 4 value))
                (g (nth 8 value))
                (b (nth 12 value))
                (a (nth 16 value)))
            (values rest (rgba r g b a) t))
          (values input nil nil)))))

(defun parse-rgba (string)
  (nth-value 1 (parse (.rgba) string)))

(defun color (value)
  (let ((value (typecase value
                 ((or rgb rgba) value)
                 (keyword (cond
                            ((eq value :transparent) (rgba 0 0 0 0))
                            ((eq value :inherit) value)
                            (t (loop for (name nil (r g b)) in *colors*
                                  when (eq value name)
                                  do (return (rgba r g b 1))))))
                 (string (or (and (string-equal value "transparent")
                                  (rgba 0 0 0 0))
                             (and (string-equal value "inherit")
                                  :inherit)
                             (parse-rgb value)
                             (parse-rgba value)
                             (parse-color-hex value)
                             (loop for (name nil (r g b)) in *colors*
                                when (string-equal value (symbol-name name))
                                do (return (rgba r g b 1)))
                             (error "Unable to parse color value ~S" value)))
                 (t (error "Bad color value ~A" value)))))
    (make-instance 'color :value value)))

(defun opacity (value)
  (let ((value (typecase value
                 (number (if (<= 0 value 1) value (error "Bad opacity value ~A" value)))
                 (keyword (if (eq value :inherit)
                              value
                              (error "Bad opacity value ~A" value)))
                 (string (or (parse-alpha value)
                             (and (string-equal value "inherit") :inherit)
                             (error "Bad opacity value ~A" value))))))
    (make-instance 'opacity :value value)))
