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
       when (and (member part '("none" "hidden" "dotted" "dashed"
                                "solid" "double" "groove" "ridge"
                                "inset" "outset")
                         :test 'equal)
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

(define-property border-top-width ()
  ()
  (:value length :thin :medium :thick))

(define-property border-right-width ()
  ()
  (:value length :thin :medium :thick))

(define-property border-bottom-width ()
  ()
  (:value length :thin :medium :thick))

(define-property border-left-width ()
  ()
  (:value length :thin :medium :thick))

(defun parse-border-width (string)
  (let ((parts (split-sequence #\Space string)))
    (loop for part in parts
       for i from 0
       when (and (or (member part '("thin" "medium" "thick" )
                             :test 'equal)
                     (parse-length part))
                 (< i 4))
       collect part
       else do (error "Bad border-width value ~S" string))))

(defun border-width (&rest values)
  (case (cl:length values)
    (1 (let ((value (first values)))
         (typecase value
           (string (or (apply #'border-width (parse-border-width value))
                       (error "Bad border-width value ~S" values)))
           (keyword `(,(border-top-width value)
                       ,(border-right-width value)
                       ,(border-bottom-width value)
                       ,(border-left-width value)))
           (t (error "Bad border-width value ~A" value)))))
    (2 `(,(border-top-width (first values))
          ,(border-right-width (second values))
          ,(border-bottom-width (first values))
          ,(border-left-width (second values))))
    (3 `(,(border-top-width (first values))
          ,(border-right-width (second values))
          ,(border-bottom-width (third values))
          ,(border-left-width (second values))))
    (4 `(,(border-top-width (first values))
          ,(border-right-width (second values))
          ,(border-bottom-width (third values))
          ,(border-left-width (fourth values))))
    (t (error "Bad border-width values ~A" values))))
