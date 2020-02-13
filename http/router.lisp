(in-package :http)

(define-handler router ()
  ((routes
    :initarg :routes
    :initform nil
    :accessor router-routes))
  (:instanize nil)
  (:function (lambda (router request)
               (let ((target-route nil))
                 (loop for route in (router-routes router)
                    when (route-match-p route request)
                    do
                      (setf target-route route)
                      (return))
                 (if target-route
                     (if-let ((handler (route-handler target-route)))
                       (if (boundp handler)
                           (progn
                             (setf handler (symbol-value handler))
                             (check-type handler handler)
                             (invoke-handler handler request))
                           (error "Handler not bound"))
                       (handle-missing request)))))))

(defclass route ()
  ((matcher
    :initarg :matcher
    :initform nil
    :accessor route-matcher)
   (handler
    :initarg :handler
    :initform nil
    :accessor route-handler)))

(defgeneric route (type form))

(defun route-match-p (route request)
  (let ((matcher (route-matcher route)))
    (funcall matcher request)))

(defclass simple-route (route)
  ((method
    :initarg :method
    :initform nil
    :accessor route-method)
   (path
    :initarg :path
    :initform nil
    :accessor route-path)))

(defmethod route ((type (eql :get)) form)
  (apply 'make-simple-route form))
(defmethod route ((type (eql :post)) form)
  (apply 'make-simple-route form))
(defmethod route ((type (eql :put)) form)
  (apply 'make-simple-route form))
(defmethod route ((type (eql :delete)) form)
  (apply 'make-simple-route form))

(defun make-simple-route (method path handler)
  (let ((matcher (lambda (request)
                   (let ((uri (uri (request-uri request))))
                     (and (equal path (uri-path uri))
                          (equal (symbol-name method)
                                 (request-method request)))))))
    (make-instance 'simple-route
                   :method method
                   :path path
                   :matcher matcher
                   :handler handler)))

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
                  (unless (find-method #'route
                                       '()
                                       (list `(eql ,type)
                                             (find-class t))
                                       nil)
                    (error "No method found to build route for form ~A" route-form))
                  `(route ,type ',route-form))))))
    `(make-instance 'router :routes (list ,@make-route-forms))))

(defun handle-missing (request)
  (declare (ignore request))
  (reply
   (status 404)
   "not found"))
