(in-package :css)

;; https://drafts.csswg.org/css-flexbox-1

;; TODO: flex shorthand

(define-property flex-direction ()
  ()
  (:value :row :row-reverse :column :column-reverse))

(define-property flex-wrap ()
  ()
  (:value :nowrap :wrap :wrap-reverse))

(define-property flex-flow () ())

(defun parse-flex-flow (string)
  (loop for part in (split-sequence #\Space string)
     for i from 1
     when (or (and (= i 1)
                   (member part '("row" "row-reverse" "column" "column-reverse")
                           :test 'equal))
              (and (= i 2)
                   (member part '("nowrap" "wrap" "wrap-reverse")
                           :test 'equal)))
     collect (make-keyword (string-upcase part))
     else do (error "Bad flex-flow value ~S" string)))

(defun flex-flow (&rest values)
  (let ((value (case (cl:length values)
                 (1 (let ((value (first values)))
                      (typecase value
                        (string (parse-flex-flow value))
                        (keyword (if (member value '(:row :row-reverse
                                                     :column :column-reverse
                                                     :nowrap :wrap :wrap-reverse))
                                     value
                                     (error "Bad flow-flow values ~A" values)))
                        (t (error "Bad flex-flow values ~A" values)))))
                 (2 values))))
    (make-instance 'flex-flow :value value)))

;; (flex-flow "row")
;; (flex-flow "column wrap")
;; (flex-flow "row-reverse wrap-reverse")

(define-property order ()
  ()
  (:value integer))

(define-property flex-grow ()
  ()
  (:value number))

(define-property flex-shrink ()
  ()
  (:value number))

(define-property flex-basis ()
  ()
  ;; TODO: https://drafts.csswg.org/css-flexbox-1/#flex-basis-property
  (:value :content length percentage :auto :inherit))

(define-property justify-content ()
  ()
  (:value :flex-start :flex-end :center :space-between :space-around))

(define-property align-items ()
  ()
  (:value :flex-start :flex-end :center :baseline :stretch))

(define-property align-self ()
  ()
  (:value :auto :flex-start :flex-end :center :baseline :stretch))

(define-property align-content ()
  ()
  (:value :flex-start :flex-end :center :space-between :space-around :stretch))
