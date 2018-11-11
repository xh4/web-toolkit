(in-package :wt.form)

(defun tag-body-parts (form)
  "Pull the attributes off the front of BODY and return the attributes
and the body."
  (let ((body (loop for rest on form by #'cddr
                 unless (keywordp (car rest))
                 return rest)))
    (values (ldiff form body) body)))

(defun uri-filename (thing)
  (bind (((:values _ _ _ _ path _ _) (quri:parse-uri thing))
         (pathname (pathname path)))
    (format nil "~A.~A" (pathname-name pathname) (pathname-type pathname))))

(defclass form ()
  ((flavor :initarg :flavor :initform :common :accessor form-flavor)
   (action :initarg :action :initform nil :accessor form-action)
   (method :initarg :method :initform nil :accessor form-method)
   (enctype :reader form-enctype)
   (fields :initarg :fields :initform nil :accessor form-fields)))

(defclass field ()
  ((tag :initarg :tag :initform nil :accessor field-tag)
   (attributes :initarg :attributes :initform nil :accessor field-attributes)
   (body :initarg :body :initform nil :accessor field-body)))

(defmethod form-enctype ((form form))
  (if (some (lambda (field) (eq (field-tag field) :file)) (form-fields form))
      "multipart/form-data"
      "application/x-www-form-urlencoded"))

(defgeneric make-form (flavor form value))

(defgeneric make-field (flavor tag field value))

(defgeneric make-field-label (flavor tag field))

(defgeneric make-field-control (flavor tag field value))

(defgeneric prepare-field-attributes (flavor tag attributes value))

(defmethod make-field-label (flavor tag field)
  (declare (ignore tag))
  (bind (((:plist label) (field-attributes field)))
    `((:label ,label))))

(defmethod prepare-field-attributes (flavor tag attributes value)
  attributes)

(defmethod make-field-control (flavor (tag (eql :text)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist name disabled readonly) attributes))
    `((:input :type "text"
              :name ,name
              :disabled ,disabled
              :readonly ,readonly
              :value ,(or value (getf attributes :value))))))

(defmethod make-field-control (flavor (tag (eql :textarea)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist name) attributes))
    `((:textarea :name ,name
                 ,(or value (getf attributes :value))))))

(defmethod make-field-control (flavor (tag (eql :select)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist disabled) attributes))
    `((:select :disabled ,disabled
               ,@(loop for form in (field-body field)
                    append
                      (multiple-value-bind (attributes body) (tag-body-parts form)
                        (declare (ignore body))
                        (bind (((:plist option disabled) attributes)
                               (option-value (getf attributes :value)))
                          `((:option :value ,option-value
                                     :disabled ,disabled
                                     :selected ,(equal value option-value)
                                     ,option)))))))))

(defmethod make-field-control (flavor (tag (eql :radio)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist name) attributes)
         (body (field-body field)))
    (loop for item in body
       append
         (bind (((:plist label disabled checked) item)
                (option-value (or (getf item :value) label)))
           `((:input :type "radio"
                     :name ,name
                     :disabled ,disabled
                     :value ,option-value
                     :checked ,(if value
                                   (string-equal value option-value)
                                   checked))
             (:label ,label))))))

(defmethod make-field-control (flavor (tag (eql :checkbox)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist name disabled) attributes)
         (body (field-body field)))
    `((:input :name ,name
              :type :checkbox
              :disabled ,disabled
              :checked ,(and (true value) (not (eq value :null))))
      (:label ,body))))

(defmethod make-field-control (flavor (tag (eql :hidden)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist name) attributes))
    `((:input :type "hidden" :name ,name :value ,(or value (getf attributes :value))))))

(defmethod make-field-control (flavor (tag (eql :file)) field value)
  (bind ((attributes (field-attributes field))
         ((:plist name accept multiple) attributes))
    `(,(when (stringp value)
         `(:a :href ,value :target "_blank" ,(uri-filename value)))
      (:input :type "file" :name ,name :accept ,accept :multiple ,multiple))))

(defmethod make-field-control (flavor (tag (eql :submit)) field value)
  (declare (ignore value))
  (bind ((attributes (field-attributes field))
         ((:plist class style) attributes)
         (body (field-body field)))
    `((:button :type "submit"
               :class ,class
               :style ,style
               ,@body))))

(defmethod make-field (flavor tag field value)
  (let ((label-forms (make-field-label flavor tag field))
        (control-forms (make-field-control flavor (field-tag field) field value)))
    `((:div :class "field"
            ,@label-forms
            ,@control-forms))))

(defun make-fields (flavor form form-value)
  (loop for field in (form-fields form)
     for tag = (field-tag field)
     for attributes  = (field-attributes field)
     for body = (field-body field)
     for name = (getf attributes :name)
     for field-value = (getf form-value name)
     append (make-field flavor tag field field-value)))

(defmethod make-form (flavor form value)
  `(:form :method ,(form-method form)
          :action ,(form-action form)
          :enctype ,(form-enctype form)
          ,@(make-fields flavor form value)))

(defun render-form (form &optional value)
  (make-form (form-flavor form) form value))

(defmacro define-form (name &rest body)
  (multiple-value-bind (attributes body) (tag-body-parts body)
    (bind (((:plist (flavor _ :common) action method) attributes))
      `(defparameter ,name
         (make-instance 'form :flavor ,flavor :action ,action :method ,method
                        :fields (list ,@(loop for form in body
                                           collect
                                             (let ((tag (first form)))
                                               (multiple-value-bind (attributes body) (tag-body-parts (rest form))
                                                 `(make-instance 'field :tag ,tag :attributes ',attributes :body ',body))))))))))



(defun test-form-builder ()
  (macroexpand-1 '(define-form myform
                   :action "/foo" :method :post
                   (:text :label "Name" :name :name)
                   (:text :label "Age" :name :age :value 18)
                   (:select :label "Gender" :name :gender
                    (:option "Male" :value "male")
                    (:option "Female" :value "female"))
                   (:radio :label "Married" :name :married
                    (:label "Yes" :disabled t)
                    (:label "No" :disabled t :checked t)))))
