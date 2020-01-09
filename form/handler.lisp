(in-package :form)

(define-handler form-handler ()
  ((form-name
    :initarg :form-name
    :initform nil
    :accessor handler-form-name)))

(defmethod handle ((handler form-handler) request)
  (call-next-handler))



(define-handler form-render-handler (form-handler)
  ())

(defmethod handle ((handler form-render-handler) (request request))
  (with-slots (form-name) handler
    (let ((form-definition (eval form-name)))
      (setf (response-status *response*) 200
            (header-field *response* "Content-Type") "text/html")
      ;; TODO: Handle error when construct and render form
      (let ((form (funcall form-name)))
        (let ((body (html:serialize
                     (html:document
                      (html:html
                       (html:head
                        (html:meta :charset "utf-8"))
                       (html:body
                        (render form)))))))
          (setf (response-body *response*) body))))))


(defun request-query-params (request)
  (uri:uri-query (request-uri request) :type 'alist))

(defun request-content-type (request)
  (let ((header (request-header request)))
    (header-field-value (header-field header "Content-Type"))))

(defun read-request-body-to-string (request)
  (let ((body (request-body request)))
    (alexandria::read-stream-content-into-string body)))

(defun read-request-form-data (request)
  (let ((content-type (request-content-type request)))
    (when (search "application/x-www-form-urlencoded" content-type
                  :test 'string-equal)
      (let ((body (read-request-body-to-string request)))
        (when body
          (uri:uri-query (uri:uri :query body) :type :alist))))))

;; hunchentoot request.lisp parse-multipart-form-data

(defun read-request-multipart-form-data (request)
  (let ((content-type (request-content-type request)))
    (when (search "multipart/form-data" content-type
                  :test 'string-equal)
      )))

(define-handler form-submit-handler (form-handler)
  ())

(defmethod handle ((handler form-submit-handler) (request request))
  (with-slots (form-name) handler
    (let ((form (funcall form-name)))

      (let ((query-params (request-query-params request))
            (body-params (read-request-form-data request)))
        (let ((params (append query-params body-params)))
          (loop for (name . value) in params
             for field = (form-field form name)
             when field
             do
               (setf (field-value field) value))))

      ;; (when (find-method #'process-form nil
      ;;                    (list (class-of form)) nil)
      ;;   (process-form form))

      (setf (response-status *response*) 200
            (header-field *response* "Content-Type") "text/html")

      (let ((body (html:serialize (render form))))
        (setf (response-body *response*) body)))))



(defmethod build-routing-rule ((type (eql :form)) routing-form)
  (declare (ignore type))
  (let ((path)
        (form-name)
        (rules))
    (match routing-form
      ((list type symbol)
       (setf form-name symbol
             path (string-downcase (symbol-name form-name))))
      ((list type path-form symbol)
       (setf path (eval path-form)
             form-name symbol)))
    ;; render
    (push
     (let ((handler (make-instance 'form-render-handler
                                   :form-name form-name))
           (matcher (lambda (request)
                      (and (equal (string-left-trim "/" (request-uri request))
                                  path)
                           (eq :get (request-method request))))))
       (make-instance 'routing-rule
                      :handler handler
                      :matcher matcher))
     rules)
    ;; submit
    (push
     (let ((handler (make-instance 'form-submit-handler
                                   :form-name form-name))
           (matcher (lambda (request)
                      (and (equal (string-left-trim "/" (request-uri request))
                                  path)
                           (eq :post (request-method request))))))
       (make-instance 'routing-rule
                      :handler handler
                      :matcher matcher))

     rules)
    (reverse rules)))
