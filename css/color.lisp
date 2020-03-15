(in-package :css)

;; https://www.w3.org/TR/css-color-3

(define-property color () ())

(defun color (&rest values)
  )

(define-property opacity () ())

(defun opacity (value))

(define-parser .alpha ()
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

;; (color "red")
;; (color :red)
;; (color "transparent")
;; (color :transparent)
;; (color (rgb 255 255 255))
;; (color "rbg(255, 255, 255)")
;; (color (rgba 255 255 255 0))
;; (color "rbga(255, 255, 255, 0)")
;; (color "#000000")


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
                          (.alpha)
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
