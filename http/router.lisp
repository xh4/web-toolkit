(in-package :http)

(define-handler router ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor router-rules)))

(defun pprint-router (router stream)
  (let ((*print-pretty* t))
    (pprint-logical-block (stream nil)
      (pprint-indent :block (indent-relative-to-object-name router 1) stream)
      (pprint-newline :mandatory stream)
      (write-string "Rules:" stream)
      (loop for rule in (router-rules router)
         do
           (pprint-indent :block (indent-relative-to-object-name router 3) stream)
           (pprint-newline :mandatory stream)
           (format stream "~A" rule)
         finally
           (pprint-indent :block (indent-relative-to-object-name router -2) stream)
           (pprint-newline :mandatory stream)))))

(defmethod print-object ((router router) stream)
  (print-unreadable-object (router stream :type t :identity t)
    (if *print-pretty*
        (pprint-router router stream))))

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
                     (and (equal (routing-rule-path rule) (request-uri request))
                          (eq (routing-rule-method rule) (request-method request))))))
      (setf (routing-rule-matcher rule) matcher))
    rule))

(defmacro router (&rest rule-forms)
  (let ((rules (loop for rule-form in rule-forms
                  append
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
                        (ensure-list (build-routing-rule type rule-form)))))))
    (make-instance 'router :rules rules)))

(defun handle-missing (request)
  (declare (ignore request))
  (setf (response-status *response*) 404)
  (setf (header-field *response* "Content-Type") "text/plain")
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
