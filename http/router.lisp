(in-package :http)

(define-handler router ()
  ((routes
    :initarg :routes
    :initform nil
    :accessor router-routes))
  (:instanize nil)
  (:function (lambda (router request)
               (let ((handler))
                 (loop for route in (router-routes router)
                    do (setf handler (route route request))
                    when handler
                    do (return))
                 (if handler
                     (typecase handler
                       (handler (invoke-handler handler request))
                       (symbol (if (boundp handler)
                                   (progn
                                     (setf handler (symbol-value handler))
                                     (check-type handler handler)
                                     (invoke-handler handler request))
                                   (error "Handler not bound"))))
                     (handle-missing request))))))

(defclass route () ())

(defgeneric route (route request))

(defgeneric make-route (type form))

(defclass simple-route (route)
  ((method
    :initarg :method
    :initform nil
    :accessor route-method)
   (path
    :initarg :path
    :initform nil
    :accessor route-path)
   (handler
    :initarg :handler
    :initform nil)))

(defun request-uri-variables (request)
  *request-uri-variables*)

(defmethod route ((route simple-route) request)
  (let ((request-uri (request-uri request)))
    (when (and (equal (symbol-name (route-method route))
                      (request-method request))
               (or (equal (route-path route) (uri-path request-uri))
                   (cl-uri-templates:with-uri-environment
                     (let ((variables (plist-alist
                                       (cl-uri-templates:destructure-uri (uri-path request-uri)
                                                                         (route-path route)))))

                       (and variables
                            (notany (lambda (v) (find #\/ (cdr v))) variables)
                            (setf *request-uri-variables* variables))))))
      (slot-value route 'handler))))

(defmethod make-route ((type (eql :get)) form)
  (apply 'make-simple-route form))
(defmethod make-route ((type (eql :post)) form)
  (apply 'make-simple-route form))
(defmethod make-route ((type (eql :put)) form)
  (apply 'make-simple-route form))
(defmethod make-route ((type (eql :delete)) form)
  (apply 'make-simple-route form))

(defun make-simple-route (method path handler)
  (make-instance 'simple-route
                 :method method
                 :path path
                 :handler (handler-form handler)))

(defmacro router (&rest route-forms)
  (let ((make-route-forms
         (loop for route-form in route-forms
            collect
              (progn
                (unless (listp route-form)
                  (error "Illformed route form: ~A, expect a list" route-form))
                (let ((type (car route-form)))
                  (unless (symbolp type)
                    (error "Illformed route form: ~A, expect a symbol at the head" route-form))
                  (setf type (make-keyword type))
                  (unless (find-method #'make-route
                                       '()
                                       (list `(eql ,type)
                                             (find-class t))
                                       nil)
                    (error "No method found to build route for form ~A" route-form))
                  `(make-route ,type ',route-form))))))
    `(make-instance 'router :routes (list ,@make-route-forms))))

(defun handle-missing (request)
  (declare (ignore request))
  (reply
   (status 404)
   "not found"))
