(in-package :http)

(define-handler router ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor router-rules))
  (:instanize nil))

(defmacro define-router (name)
  `(if (boundp ',name)
       (progn
         (setf ,name (make-instance 'router))
         ,name)
       (defvar ,name
         (make-instance 'router))))

(defclass routing-rule ()
  ((matcher
    :initarg :matcher
    :initform nil
    :accessor routing-rule-matcher)
   (handler
    :initarg :handler
    :initform nil
    :accessor routing-rule-handler)))

(defgeneric build-routing-rule (type form))

(defclass verbose-routing-rule (routing-rule)
  ((method
    :initarg :method
    :initform nil
    :accessor routing-rule-method)
   (path
    :initarg :path
    :initform nil
    :accessor routing-rule-path)))

(defmethod build-routing-rule ((type (eql :get)) form)
  (apply #'build-verbose-routing-rule form))
(defmethod build-routing-rule ((type (eql :post)) form)
  (apply #'build-verbose-routing-rule form))
(defmethod build-routing-rule ((type (eql :put)) form)
  (apply #'build-verbose-routing-rule form))
(defmethod build-routing-rule ((type (eql :delete)) form)
  (apply #'build-verbose-routing-rule form))

(defun build-verbose-routing-rule (method path handler)
  (let ((rule (make-instance 'verbose-routing-rule
                             :method method
                             :path path
                             :handler handler)))
    (let ((matcher (lambda (request)
                     (let ((uri (uri (request-uri request))))
                       (and (equal (routing-rule-path rule) (uri-path uri))
                            (equal (symbol-name (routing-rule-method rule))
                                   (request-method request)))))))
      (setf (routing-rule-matcher rule) matcher))
    rule))

(defmacro router (&rest rule-forms)
  (let ((rule-making-forms
         (loop for rule-form in rule-forms
            collect
              (progn
                (unless (listp rule-form)
                  (error "Illformed rule form: ~A, expect a list" rule-form))
                (let ((type (car rule-form)))
                  (unless (symbolp type)
                    (error "Illformed rule form: ~A, expect a symbol at the head" rule-form))
                  (setf type (make-keyword type))
                  (unless (find-method #'build-routing-rule
                                       '()
                                       (list `(eql ,type)
                                             (find-class t))
                                       nil)
                    (error "No method to build routing rule for form ~A" rule-form))
                  `(build-routing-rule ,type ',rule-form))))))
    `(make-instance 'router :rules (list ,@rule-making-forms))))

(defun handle-missing (request)
  (declare (ignore request))
  (reply
   (status 404)
   (header :content-type "text/plain")
   "not found"))

(defmethod handle ((router router) (request request))
  (let ((target-rule nil))
    (loop for rule in (router-rules router)
       for matcher = (routing-rule-matcher rule)
       when (funcall matcher request)
       do
         (setf target-rule rule)
         (return))
    (if target-rule
        (let ((handler (symbol-value (routing-rule-handler target-rule))))
          (typecase handler
            (handler (invoke-handler handler request))
            (t (handle handler request))))
        (handle-missing request))))
