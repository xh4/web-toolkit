(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3/#borders

(define-property border-color () ())

(defun border-color (value)
  (if-let ((value (typecase value
                    (string (parse-color value))
                    ((or rgb rgba) value))))
    (make-instance 'border-color :value value)
    (error "Bad border-color value ~A" value)))

(define-property border-top-style ()
  ()
  (:value :none :hidden :dotted :dashed :solid :double
          :groove :ridge :inset :outset))

(define-property border-right-style ()
  ()
  (:value :none :hidden :dotted :dashed :solid :double
          :groove :ridge :inset :outset))

(define-property border-bottom-style ()
  ()
  (:value :none :hidden :dotted :dashed :solid :double
          :groove :ridge :inset :outset))

(define-property border-left-style ()
  ()
  (:value :none :hidden :dotted :dashed :solid :double
          :groove :ridge :inset :outset))

(defun parse-border-style (string)
  (let ((parts (split-sequence #\Space string)))
    (loop for part in parts
         for i from 0
       when (and (member part '("none" "hidden" "dotted" "dashed" "solid" "double"
                                "groove" "ridge" "inset" "outset") :test 'equal)
                 (< i 4))
       collect part
       else do (error "Bad border-style value ~S" string))))

(defun border-style (&rest values)
  (case (cl:length values)
    (1 (let ((value (first values)))
         (typecase value
           (string (or (apply #'border-style (parse-border-style value))
                       (error "Bad border-style value ~S" values)))
           (keyword `(,(border-top-style value)
                       ,(border-right-style value)
                       ,(border-bottom-style value)
                       ,(border-left-style value)))
           (t (error "Bad border-style value ~A" value)))))
    (2 `(,(border-top-style (first values))
          ,(border-right-style (second values))
          ,(border-bottom-style (first values))
          ,(border-left-style (second values))))
    (3 `(,(border-top-style (first values))
          ,(border-right-style (second values))
          ,(border-bottom-style (third values))
          ,(border-left-style (second values))))
    (4 `(,(border-top-style (first values))
          ,(border-right-style (second values))
          ,(border-bottom-style (third values))
          ,(border-left-style (fourth values))))
    (t (error "Bad border-style values ~A" values))))
