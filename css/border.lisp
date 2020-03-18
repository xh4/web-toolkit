(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3/#borders

(defmacro define-border-color-property (property-name)
  `(define-property ,property-name () () (:value .color)))

(define-border-color-property border-top-color)

(define-border-color-property border-right-color)

(define-border-color-property border-bottom-color)

(define-border-color-property border-left-color)

(defmacro define-border-style-property (property-name)
  `(define-property ,property-name () ()
                    (:value .line-style)))

(define-parser .line-style ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.s "none") (.s "hidden") (.s "dotted") (.s "dashed") (.s "solid")
                    (.s "double") (.s "groove") (.s "ridge") (.s "inset") (.s "outset"))
               input)
      (if match-p
          (values rest (make-keyword (string-upcase value)) t)
          (values input nil nil)))))

(define-border-style-property border-top-style)

(define-border-style-property border-right-style)

(define-border-style-property border-bottom-style)

(define-border-style-property border-left-style)

(define-property border-style ()
  ()
  (:value .border-style))

(define-parser .border-style ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.line-style) (.some (.whitespace))
                          (.line-style) (.some (.whitespace))
                          (.line-style) (.some (.whitespace))
                          (.line-style))
                    (.seq (.line-style) (.some (.whitespace))
                          (.line-style) (.some (.whitespace))
                          (.line-style))
                    (.seq (.line-style) (.some (.whitespace))
                          (.line-style))
                    (.line-style))
               input)
      (if match-p
          (values rest (remove-if-not #'keywordp value) t)
          (values input nil nil)))))

;; (border-style "dotted double ridge outset")

(defmacro define-border-width-property (property-name)
  `(define-property ,property-name () ()
                    (:value .length :thin :medium :thick)))

(define-border-width-property border-top-width)

(define-border-width-property border-right-width)

(define-border-width-property border-bottom-width)

(define-border-width-property border-left-width)

;; TODO: border-width
(define-property border-width ()
  ()
  (:value .border-width))

(define-parser .line-width ()
  (.oneof (.length) (.s "thin") (.s "medium") (.s "thick")))

(define-parser .border-width ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.line-width) (.some (.whitespace))
                          (.line-width) (.some (.whitespace))
                          (.line-width) (.some (.whitespace))
                          (.line-width))
                    (.seq (.line-width) (.some (.whitespace))
                          (.line-width) (.some (.whitespace))
                          (.line-width))
                    (.seq (.line-width) (.some (.whitespace))
                          (.line-width))
                    (.line-width))
               input)
      (if match-p
          (values rest (loop for v in (flatten value)
                          when (typep v 'string)
                          collect (make-keyword (string-upcase v))
                          when (typep v 'length)
                          collect v)
                  t)
          (values input nil nil)))))

;; (border-width "1px 2px 3px 4px")

(defmacro define-border-property (property-name)
  `(define-property ,property-name () () (:value .border-side)))

(define-border-property border-top)

(define-border-property border-right)

(define-border-property border-bottom)

(define-border-property border-left)

(define-parser .border-side ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.line-width) (.some (.whitespace)) (.line-style) (.some (.whitespace)) (.color))
                    (.seq (.line-width) (.some (.whitespace)) (.color) (.some (.whitespace)) (.line-style))
                    (.seq (.line-style) (.some (.whitespace)) (.line-width) (.some (.whitespace)) (.color))
                    (.seq (.line-style) (.some (.whitespace)) (.color) (.some (.whitespace)) (.line-width))
                    (.seq (.color) (.some (.whitespace)) (.line-style) (.some (.whitespace)) (.line-width))
                    (.seq (.color) (.some (.whitespace)) (.line-width) (.some (.whitespace)) (.line-style))
                    (.seq (.line-width) (.some (.whitespace)) (.line-style))
                    (.seq (.line-width) (.some (.whitespace)) (.color))
                    (.seq (.line-style) (.some (.whitespace)) (.line-width))
                    (.seq (.line-style) (.some (.whitespace)) (.color))
                    (.seq (.color) (.some (.whitespace)) (.line-width))
                    (.seq (.color) (.some (.whitespace)) (.line-style))
                    (.line-width) (.line-style) (.color))
               input)
      (if match-p
          (values rest (loop for v in (flatten (ensure-list value))
                          when (typep v '(or length rgb rgba color-hex keyword))
                          collect v)
                  t)
          (values input nil nil)))))

(define-property border ()
  ()
  (:value .border-side))

;; (border "1px")
;; (border "1px solid #fff")
;; (border "#fff solid 1px")

(defmacro define-border-corner-radius-property (property-name)
  `(define-property ,property-name () () (:value .border-corner-radius)))

(define-border-corner-radius-property border-top-left-radius)

(define-border-corner-radius-property border-top-right-radius)

(define-border-corner-radius-property border-bottom-right-radius)

(define-border-corner-radius-property border-bottom-left-radius)

(define-parser .border-corner-radius ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.or (.seq (.or (.length) (.percentage))
                          (.some (.whitespace))
                          (.or (.length) (.percentage)))
                    (.or (.length) (.percentage)))
               input)
      (if match-p
          (values rest (loop for v in (flatten value)
                          when (typep v 'length)
                          collect v)
                  t)
          (values input nil nil)))))

(define-property border-radius ()
  ()
  (:value .border-radius))

(define-parser .border-radius ()
  (lambda (input)
    (multiple-value-bind (rest value match-p)
        (parse (.seq (.or (.seq (.or (.length) (.percentage))
                                (.some (.whitespace))
                                (.or (.length) (.percentage))
                                (.some (.whitespace))
                                (.or (.length) (.percentage))
                                (.some (.whitespace))
                                (.or (.length) (.percentage)))
                          (.seq (.or (.length) (.percentage))
                                (.some (.whitespace))
                                (.or (.length) (.percentage))
                                (.some (.whitespace))
                                (.or (.length) (.percentage)))
                          (.seq (.or (.length) (.percentage))
                                (.some (.whitespace))
                                (.or (.length) (.percentage)))
                          (.or (.length) (.percentage)))
                     (.maybe (.seq (.some (.whitespace))
                                   (.s "/")
                                   (.some (.whitespace))
                                   (.or (.seq (.or (.length) (.percentage))
                                              (.some (.whitespace))
                                              (.or (.length) (.percentage))
                                              (.some (.whitespace))
                                              (.or (.length) (.percentage))
                                              (.some (.whitespace))
                                              (.or (.length) (.percentage)))
                                        (.seq (.or (.length) (.percentage))
                                              (.some (.whitespace))
                                              (.or (.length) (.percentage))
                                              (.some (.whitespace))
                                              (.or (.length) (.percentage)))
                                        (.seq (.or (.length) (.percentage))
                                              (.some (.whitespace))
                                              (.or (.length) (.percentage)))
                                        (.or (.length) (.percentage))))))
               input)
      (if match-p
          (values rest (loop for layer in value
                          collect (loop for v in (flatten layer)
                                     when (typep v 'length)
                                     append `(,v)))
                  t)
          (values input nil nil)))))

;; (border-radius "2em 1em 4em / 0.5em 3em")
;; (border-radius "1px 2px 3px 4px / 1px 2px 3px 4px")

;; TODO: border images
;; https://drafts.csswg.org/css-backgrounds-3/#border-images
