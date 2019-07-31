(in-package :http)

(define-handler router ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor router-rules)))

(defmethod print-object ((router router) stream)
  (print-unreadable-object (router stream :type t :identity t)
    (loop for rule in (router-rules router)
         do (format stream "~%  ~A" rule))))

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
                             :handler (gethash handler *handler-mapping-table*))))
    (let ((matcher (lambda (request)
                     (unless (equal (routing-rule-path rule) (request-uri request))
                       (format t "Path: ~A | ~A~%"
                               (routing-rule-path rule)
                               (request-uri request)))
                     (unless (eq (routing-rule-method rule) (request-method request))
                       (format t "Method: ~A | ~A~%"
                               (routing-rule-method rule)
                               (request-method request)))
                     (and (equal (routing-rule-path rule) (request-uri request))
                          (eq (routing-rule-method rule) (request-method request))))))
      (setf (routing-rule-matcher rule) matcher))
    rule))

(defmacro router (&rest rule-forms)
  (let ((rules (loop for rule-form in rule-forms
                  collect (build-routing-rule
                           (make-keyword (car rule-form))
                           rule-form))))
    (make-instance 'router :rules rules)))

(defun handle-missing (request)
  (declare (ignore request))
  (setf (response-status *response*) 404)
  (appendf (response-header *response*)
           (list (make-instance 'header-field
                                :name "Content-Type"
                                :value "text/plain")))
  (setf (response-body *response*) "not found"))

(defmethod handle ((router router) (request request))
  (let ((target-rule nil))
    (loop for rule in (router-rules router)
       for matcher = (routing-rule-matcher rule)
       when (funcall matcher request)
       do
         (setf target-rule rule)
         (return))
    (if target-rule
        (let ((handler (routing-rule-handler target-rule)))
          (invoke-handler handler request))
        (handle-missing request))))
