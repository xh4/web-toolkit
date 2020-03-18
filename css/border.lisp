(in-package :css)

;; https://drafts.csswg.org/css-backgrounds-3/#borders

(defmacro define-border-color-property (property-name)
  `(progn
     (define-property ,property-name () ())
     (defun ,property-name (value)
       (if-let ((value (typecase value
                         (string (parse-color value))
                         ((or rgb rgba) value))))
         (make-instance ',property-name :value value)
         (error "Bad ~A value ~A" ',property-name value)))))

(define-border-color-property border-top-color)

(define-border-color-property border-right-color)

(define-border-color-property border-bottom-color)

(define-border-color-property border-left-color)

(defun parse-border-color (string)
  (loop for part in (split-sequence #\Space string)
     for i from 0
     when (< i 4)
     collect (or (parse-color part)
                 (error "Bad border-color value ~S" string))))

(defun border-color (&rest values)
  (case (cl:length values)
    (1 (let ((value (first values)))
         (typecase value
           (string (apply #'border-color (parse-border-color value)))
           ((or rgb rgba) `(,(border-top-color value)
                             ,(border-right-color value)
                             ,(border-bottom-color value)
                             ,(border-left-color value)))
           (t (error "Bad border-color value ~A" value)))))
    (2 `(,(border-top-color (first values))
          ,(border-right-color (second values))
          ,(border-bottom-color (first values))
          ,(border-left-color (second values))))
    (3 `(,(border-top-color (first values))
          ,(border-right-color (second values))
          ,(border-bottom-color (third values))
          ,(border-left-color (second values))))
    (4 `(,(border-top-color (first values))
          ,(border-right-color (second values))
          ,(border-bottom-color (third values))
          ,(border-left-color (fourth values))))))

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

(defun parse-border-cornor-radius (string)
  (let ((parts (split-sequence #\Space string)))
    (loop for part in parts
       for i from 0
       when (< i 4)
       collect (or (parse-length part)
                   (parse-percentage part)
                   (error (error "Bad border-corner-radius value ~S" string)))
       else do (error "Bad border-corner-radius value ~S" string))))

(define-property border-top-left-radius () ())

(defun border-top-left-radius (&rest values)
  (make-instance
   'border-top-left-radius
   :value
   (case (cl:length values)
     (1 (let ((value (first values)))
          (typecase value
            (string (or (apply #'border-top-left-radius
                               (parse-border-cornor-radius value))
                        (error "Bad border-top-left-radius value ~S" values)))
            ((or length percentage) value)
            (t (error "Bad border-top-left-radius value ~A" value)))))
     ;; TODO: check-type
     (2 values)
     (t (error "Bad border-top-left-radius values ~A" values)))))

(define-property border-top-right-radius () ())

(defun border-top-right-radius (&rest values)
  (make-instance
   'border-top-right-radius
   :value
   (case (cl:length values)
     (1 (let ((value (first values)))
          (typecase value
            (string (or (apply #'border-top-right-radius
                               (parse-border-cornor-radius value))
                        (error "Bad border-top-right-radius value ~S" values)))
            ((or length percentage) value)
            (t (error "Bad border-top-right-radius value ~A" value)))))
     ;; TODO: check-type
     (2 values)
     (t (error "Bad border-top-right-radius values ~A" values)))))

(define-property border-bottom-right-radius () ())

(defun border-bottom-right-radius (&rest values)
  (make-instance
   'border-bottom-right-radius
   :value
   (case (cl:length values)
     (1 (let ((value (first values)))
          (typecase value
            (string (or (apply #'border-bottom-right-radius
                               (parse-border-cornor-radius value))
                        (error "Bad border-bottom-right-radius value ~S" values)))
            ((or length percentage) value)
            (t (error "Bad border-bottom-right-radius value ~A" value)))))
     ;; TODO: check-type
     (2 values)
     (t (error "Bad border-bottom-right-radius values ~A" values)))))

(define-property border-bottom-left-radius () ())

(defun border-bottom-left-radius (&rest values)
  (make-instance
   'border-bottom-left-radius
   :value
   (case (cl:length values)
     (1 (let ((value (first values)))
          (typecase value
            (string (or (apply #'border-bottom-left-radius
                               (parse-border-cornor-radius value))
                        (error "Bad border-bottom-left-radius value ~S" values)))
            ((or length percentage) value)
            (t (error "Bad border-bottom-left-radius value ~A" value)))))
     ;; TODO: check-type
     (2 values)
     (t (error "Bad border-bottom-left-radius values ~A" values)))))

(defun parse-border-radius (string)
  (let* ((parts (split-sequence #\/ string))
         (horizontal-radius (first parts))
         (vertical-radius (second parts)))
    (unless vertical-radius (setf vertical-radius horizontal-radius))
    (loop for string in `(,horizontal-radius ,vertical-radius)
       for i from 0
       for parts = (split-sequence #\Space string)
       for values = (loop for part in parts
                       for j from 0
                       when (= 0 (cl:length part))
                       do (decf j) (continue)
                       else when (< j 4)
                       collect (or (parse-length part)
                                   (parse-percentage part)
                                   (error "Bad border-radius value ~S" string))
                       else do (error "Bad border-radius value ~S" string))
       when (< i 2)
       collect values
       else do (error "Bad border-radius value ~S" string))))

;; (parse-border-radius "2em 1em 4em / 0.5em 3em")

;; TODO: validate
(defun border-radius (&rest values)
  (typecase (first values)
    (string (apply #'border-radius (parse-border-radius (first values))))
    (length (case (cl:length values)
              (1 `(,(border-top-left-radius (first values))
                    ,(border-top-right-radius (first values))
                    ,(border-bottom-right-radius (first values))
                    ,(border-bottom-left-radius (first values))))
              (2 `(,(border-top-left-radius (first values))
                    ,(border-top-right-radius (second values))
                    ,(border-bottom-right-radius (first values))
                    ,(border-bottom-left-radius (second values))))
              (3 `(,(border-top-left-radius (first values))
                    ,(border-top-right-radius (second values))
                    ,(border-bottom-right-radius (third values))
                    ,(border-bottom-left-radius (second values))))
              (4 `(,(border-top-left-radius (first values))
                    ,(border-top-right-radius (second values))
                    ,(border-bottom-right-radius (third values))
                    ,(border-bottom-left-radius (fourth values))))))
    (list (let ((horizontal-radius (first values))
                (vertical-radius (second values)))
            (case (cl:length horizontal-radius)
              (1 (appendf horizontal-radius `(,(first horizontal-radius)
                                               ,(first horizontal-radius)
                                               ,(first horizontal-radius))))
              (2 (appendf horizontal-radius `(,(first horizontal-radius)
                                               ,(second horizontal-radius))))
              (3 (appendf horizontal-radius `(,(second horizontal-radius)))))
            (case (cl:length vertical-radius)
              (1 (appendf vertical-radius `(,(first vertical-radius)
                                             ,(first vertical-radius)
                                             ,(first vertical-radius))))
              (2 (appendf vertical-radius `(,(first vertical-radius)
                                             ,(second vertical-radius))))
              (3 (appendf vertical-radius `(,(second vertical-radius)))))
            (let ((h1 (first horizontal-radius))
                  (h2 (second horizontal-radius))
                  (h3 (third horizontal-radius))
                  (h4 (fourth horizontal-radius))
                  (v1 (first vertical-radius))
                  (v2 (second vertical-radius))
                  (v3 (third vertical-radius))
                  (v4 (fourth vertical-radius)))
              `(,(border-top-left-radius h1 v1)
                 ,(border-top-right-radius h2 v2)
                 ,(border-bottom-right-radius h3 v3)
                 ,(border-bottom-left-radius h4 v4)))))))

;; (border-radius "2em 1em 4em / 0.5em 3em")
;; (border-radius "1px 2px 3px 4px / 1px 2px 3px 4px")
;; (border-radius (px 1) (px 2) (px 3) (px 4))
;; (border-radius `(,(px 1) ,(px 2) ,(px 3) ,(px 4))
;;                `(,(px 1) ,(px 2) ,(px 3) ,(px 4)))

;; TODO: border images
;; https://drafts.csswg.org/css-backgrounds-3/#border-images
