(in-package :http)

(define-handler router ()
  ((routes
    :initarg :routes
    :initform nil
    :accessor router-routes))
  (:instanize nil)
  (:function (lambda (router request)
               (if-let ((result (match-route router request)))
                 (let ((handler (getf result :handler))
                       (*request-uri-variables* (getf result :uri-variables)))
                   (typecase handler
                     (handler (invoke-handler handler request))
                     (symbol (if (boundp handler)
                                 (progn
                                   (setf handler (symbol-value handler))
                                   (check-type handler handler)
                                   (invoke-handler handler request))
                                 (error "Handler not bound")))))
                 (handle-missing request)))))

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
   (regex
    :initarg :regex
    :initform nil
    :accessor route-regex)
   (variable-names
    :initarg :variable-names
    :initform nil
    :accessor route-variable-names)
   (handler
    :initarg :handler
    :initform nil)))

(defun request-uri-variables (request)
  *request-uri-variables*)

(defun request-uri-variable (request name)
  (loop for (name0 . value) in (request-uri-variables request)
     when (equal name name0)
     do (return value)))

(defmethod route ((route simple-route) request)
  (when (equal (symbol-name (route-method route))
               (request-method request))
    (if-let ((variable-names (route-variable-names route)))
      (multiple-value-bind (match variable-values)
          (cl-ppcre:scan-to-strings (route-regex route) (uri-path request))
        (when match
          (loop for name in variable-names
             for value across variable-values
             for decoded = (uri::percent-decode value)
             collect (cons name decoded) into variables
             finally (setf *request-uri-variables* variables))
          (slot-value route 'handler)))
      (when (string-equal (route-path route) (uri-path (request-uri request)))
        (slot-value route 'handler)))))

(defun match-route (router request)
  (loop with *request-uri-variables* = '()
     for route in (router-routes router)
     for handler = (route route request)
     when handler
     do (return (list :handler handler :route route :uri-variables *request-uri-variables*))))

(defmethod make-route ((type (eql :get)) form)
  (apply 'make-simple-route form))
(defmethod make-route ((type (eql :post)) form)
  (apply 'make-simple-route form))
(defmethod make-route ((type (eql :put)) form)
  (apply 'make-simple-route form))
(defmethod make-route ((type (eql :delete)) form)
  (apply 'make-simple-route form))

(defun make-simple-route (method path handler)
  (let* ((variable-names '())
         (regex (concatenate
                 'string
                 (cl-ppcre:regex-replace-all
                  "{[^}]*}"
                  path
                  (lambda (target-string start end match-start match-end reg-starts reg-ends)
                    (declare (ignore start end reg-starts reg-ends))
                    (let* ((match (subseq target-string match-start match-end))
                           (variable-name (subseq match 1 (1- (length match)))))
                      (push variable-name variable-names)
                      "([^/]+)")))
                 "$")))
    (nreversef variable-names)
    (make-instance 'simple-route
                   :method method
                   :path path
                   :regex regex
                   :variable-names variable-names
                   :handler (handler-form handler))))

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
