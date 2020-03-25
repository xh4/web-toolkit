(in-package :documentation)

(define-component symbol/o (addressable)
  ((type
    :initarg :type
    :initform :symbol)
   (symbol
    :initarg :symbol
    :initform nil)
   (chapter
    :initform nil)))

(defmethod initialize-instance :after ((symbol/o symbol/o) &key)
  (setf (title symbol/o) (format nil "~(~A~)"
                                 (slot-value symbol/o 'symbol)))
  (setf (id symbol/o) (format nil "~(~A~)/~(~A~)"
                              (slot-value symbol/o 'type)
                              (slot-value symbol/o 'symbol))))

(defmethod id ((symbol/o symbol/o))
  (format nil "~A/~A" (id (slot-value symbol/o 'chapter)) (slot-value symbol/o 'id)))

(defun superclasses (class)
  (when (symbolp class)
    (setf class (find-class class)))
  (let ((classes (compute-class-precedence-list class)))
    (subseq classes 1 (1- (length classes)))))

(defun superclasses/o (class)
  (let ((list '()))
    (loop for superclass in (superclasses class)
       unless (emptyp list)
       do (appendf list (list (br)))
       do (let* ((superclass-name (class-name superclass))
                 (superclass-package-name (package-name
                                           (symbol-package superclass-name))))
            (unless (equal "SB-PCL"
                           superclass-package-name)
              (appendf list (list (span
                                   (when (and
                                          (not (equal superclass-package-name
                                                      (package-name
                                                       (symbol-package
                                                        (class-name class)))))
                                          (not (equal "COMMON-LISP"
                                                      superclass-package-name)))
                                     (list
                                      (span :class "superclass-package-name"
                                            (string-downcase superclass-package-name))
                                      (span :class "symbol-colon" ":")))
                                   (span :class "superclass-name"
                                         (string-downcase
                                          (symbol-name superclass-name))))))))
       finally (return list))))

(define-component class/o (symbol/o)
  ((type
    :initform :class)
   (summary
    :initarg :summary
    :initform nil))
  (:render
   (lambda (c)
     (with-slots (symbol summary no title id) c
       (let ((class (find-class symbol)))
         (div
          (h3
           :id (id c)
           :class "heading"
           (span :class "secno" (format nil "~{~A~^.~}" no)) " "
           (span :class "content"
                 (span :class "symbol-type" "Class") " "
                 (span :class "symbol"
                       (string-downcase (symbol-name (class-name class)))))
           " "
           (a :class "self-link" :href (format nil "#~A" (id c))))
          (table
           :class "def propdef symbol-table"
           (tbody
            (tr
             (th "Package")
             (td (span :class "package-name"
                       (string-downcase
                        (package-name
                         (symbol-package
                          (class-name class)))))))
            (tr
             (th "Superclasses")
             (td (superclasses/o class)))
            (tr
             (th "Metaclass")
             (td (let ((metaclass (class-of class)))
                   (string-downcase
                    (symbol-name
                     (class-name metaclass))))))))
          summary))))))

(define-component accessor/o (symbol/o)
  ((type
    :initform :accessor)
   (summary
    :initarg :summary
    :initform nil))
  (:render
   (lambda (c)
     (with-slots (symbol summary id no title) c
       (div
        (h3
         :id (id c)
         :class "heading"
         (span :class "secno" (format nil "~{~A~^.~}" no)) " "
         (span :class "content"
               (span :class "symbol-type" "Accessor") " "
               (span :class "symbol"
                     (string-downcase (symbol-name symbol))))
         " "
         (a :class "self-link" :href (format nil "#~A" (id c))))
        summary)))))

(define-component function/o (symbol/o)
  ((summary
    :initarg :summary
    :initform nil))
  (:render
   (lambda (c)
     (with-slots (symbol summary id no title) c
       (div
        (h3
         :id (id c)
         :class "heading"
         (span :class "secno" (format nil "~{~A~^.~}" no)) " "
         (span :class "content"
               (span :class "symbol-type" "Function") " "
               (span :class "symbol"
                     (string-downcase (symbol-name symbol))))
         " "
         (a :class "self-link" :href (format nil "#~A" (id c))))
        summary)))))

(define-component constant/o (symbol/o)
  ((summary
    :initarg :summary
    :initform nil))
  (:render
   (lambda (c)
     (with-slots (symbol summary id no title) c
       (div
        (h3
         :id (id c)
         :class "heading"
         (span :class "secno" (format nil "~{~A~^.~}" no)) " "
         (span :class "content"
               (span :class "symbol-type" "Constant") " "
               (span :class "symbol"
                     (string-downcase (symbol-name symbol))))
         " "
         (a :class "self-link" :href (format nil "#~A" (id c))))
        summary)))))

(defun class-ref (symbol &optional text)
  (let ((package (symbol-package symbol)))
    (a :href (format nil "#~(~A~)/class/~(~A~)" (package-name package) (symbol-name symbol))
       (or text
           (format nil "~(~A~)" (symbol-name symbol))))))
