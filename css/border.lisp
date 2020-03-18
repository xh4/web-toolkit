(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3/#borders

(define-property border-color () ())

(defun border-color (value)
  (if-let ((value (typecase value
                    (string (parse-color value))
                    ((or rgb rgba) value))))
    (make-instance 'border-color :value value)
    (error "Bad border-color value ~A" value)))
