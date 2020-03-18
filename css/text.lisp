(in-package :css)

;; https://drafts.csswg.org/css-text-3

(define-property text-transform () ())

(define-parser .text-transform ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.s "none")
                    (.choose (.seq (.maybe (.whitespace))
                                   (.or (.s "capitalize")
                                        (.s "uppercase")
                                        (.s "lowercase")))
                             (.seq (.maybe (.whitespace))
                                   (.s "full-width"))
                             (.seq (.maybe (.whitespace))
                                   (.s "full-size-kana"))))
         input)
      (if match-p
          (let ((value (typecase value
                         (string (make-keyword (string-upcase value)))
                         (list (mapcar (lambda (l) (make-keyword (string-upcase (second l)))) value)))))
            (values rest value t))
          (values input nil nil)))))

(defun parse-text-transform (string)
  (multiple-value-bind (rest value match-p)
      (parse (.text-transform) string)
    (when (and match-p (maxpc::input-empty-p rest))
        value)))

;; TODO: support all keywords
(defun text-transform (value)
  (typecase value
    (keyword (if (member value '(:none :capitalize :uppercase :lowercase
                                 :full-width :full-size-kana))
                 value
                 (error "Bad text-transform value ~A" value)))
    (string (or (parse-text-transform value)
                (error "Bad text-transform value ~S" value)))
    (t (error "Bad text-transform value ~A" value))))

(define-property white-space ()
  ()
  (:value :normal :pre :nowrap :pre-wrap :break-space :pre-line))

(define-property tab-size ()
  ()
  (:value number length))

(define-property word-break ()
  ()
  (:value :normal :keep-all :break-all :break-word))

(define-property line-break ()
  ()
  (:value :auto :loose :normal :strict :anywhere))

(define-property hyphens ()
  ()
  (:value :none :manual :auto))

(define-property overflow-wrap ()
  ()
  (:value :normal :break-word :anywhere))

(define-property word-wrap ()
  ()
  (:value :normal :break-word :anywhere))

(define-property text-align ()
  ()
  (:value :start :end :left :right :center :justify :match-parent :justify-all))

(define-property text-align-all ()
  ()
  (:value :start :end :left :right :center :justify :match-parent))

(define-property text-align-last ()
  ()
  (:value :auto :start :end :left :right :center :justify :match-parent))

(define-property text-justify ()
  ()
  (:value :auto :none :inter-word :inter-character))

(define-property word-spacing ()
  ()
  (:value :normal length))

(define-property letter-spacing ()
  ()
  (:value :normal length))

(define-property text-indent ()
  ())

(define-property hanging-punctuation ()
  ())
