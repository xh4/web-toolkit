(in-package :css)

;; https://drafts.csswg.org/css-overflow-3

(define-property overflow-x ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(define-property overflow-y ()
  ()
  (:value :visible :hidden :clip :scroll :auto))

(defun overflow (&rest values)
  (case (cl:length values)
    (1 (let* ((value (first values))
              (values (split-sequence #\space value)))
         (case (cl:length values)
           (1 `(,(overflow-x (first values))
                 ,(overflow-y (first values))))
           (2 `(,(overflow-x (first values))
                 ,(overflow-y (second values)))))))
    (2 `(,(overflow-x (first values))
          ,(overflow-y (second values))))))

(define-property text-overflow ()
  ()
  (:value :clip :ellipsis))
