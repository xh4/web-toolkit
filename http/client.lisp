(in-package :http)

(defun request (uri &key (method :get) header content (entity t))
  (check-request uri method header content)
  (let ((connection-manager (make-instance 'connection-manager)))
    (let ((request (make-request uri method header content)))
      (let ((connection (request-connection connection-manager request)))
        (send-request connection request)
        (let ((response (receive-response connection)))
          (release-connection connection-manager connection)
          (shutdown-connection-manager connection-manager)
          (process-response response request :entity entity)
          response)))))

;; TODO: add method related generic functions
(defun check-request (uri method header content)
  (unless (find method *methods*)
    (error "Request method ~A not supported" method))
  (when (and (find method '(:get :head :options)) content)
    (error "Request method ~A can't carry body" method)))

(defmacro define-request-method (symbol lambda-list)
  (let ((uri (first lambda-list))
        (keyword-parameters (nth-value 3 (parse-ordinary-lambda-list lambda-list))))
    `(defun ,symbol (,uri ,@(rest lambda-list))
       (request uri
                :method ,(make-keyword symbol)
                ,@(loop for ((keyword-name name) nil nil) in keyword-parameters
                      append `(,keyword-name ,name))))))

(define-request-method get (uri &key header (entity t)))

(define-request-method head (uri &key header))

(define-request-method put (uri &key header content (entity t)))

(define-request-method post (uri &key header content (entity t)))

(define-request-method delete (uri &key header content (entity t)))

(define-request-method patch (uri &key header content (entity t)))

(define-request-method options (uri &key header))

(defun make-request (uri method header content)
  (let ((uri (process-request-uri uri)))
    (let ((header (process-request-header header)))
      (unless (find-header-field "Host" header)
        (set-header-field header (header-field "Host" (uri-host uri))))
      (let ((request (make-instance 'request
                                    :method method
                                    :uri uri
                                    :version "HTTP/1.1"
                                    :header header)))
        (process-request-content request content)
        request))))

(defun process-request-uri (uri)
  (unless (uri-host uri) (error "Missing host in URI ~S" uri))
  (uri-string uri))

(defun process-request-header (header)
  (let ((new-header (make-instance 'header)))
    ;; TODO: forbid user from setting some header fields
    (typecase header
      (header (loop for header-field in (header-fields header)
                 do (set-header-field new-header header-field)))
      (list (when header
              (loop for (name . value) in header
                 do (set-header-field new-header (header-field name value))))))
    new-header))

;; TODO: use entity
(defgeneric process-request-content (request content))

(defmethod process-request-content (request (nothing null)))

(defmethod process-request-content (request (object json:object))
  (set-header-field request (header-field "Content-Type" "application/json; charset=UTF-8"))
  (setf (request-body request) (babel:string-to-octets (json:encode object)))
  (set-header-field request (header-field "Content-Length"
                                          (length (request-body request)))))

(defmethod process-request-content (request (string string))
  (unless (find-header-field "Content-Type" request)
          (set-header-field request
                            (header-field "Content-Type" "text/plain; charset=UTF-8")))
  ;; TODO: respect user-defined charset
  (setf (request-body request) (babel:string-to-octets string))
  (set-header-field request (header-field "Content-Length"
                                          (length (request-body request)))))

(defmethod process-request-content (request (data vector))
  (setf (request-body request) data)
  (set-header-field request (header-field "Content-Length"
                                          (length (request-body request))))
  (unless (find-header-field "Content-Type" request)
    (set-header-field request
                      (header-field "Content-Type" "application/octet-stream"))))

(defmethod process-request-content (request (pathname pathname))
  )

(defmethod process-request-content (request (stream stream))
  )

(defun process-response (response request &key (entity t))
  (when (or (find (request-method request) '(:head :put))
            (not (message-body-present-p response)))
    (setf (response-body response) nil)
    (return-from process-response response))
  (let ((content-type (header-field-value
                       (find-header-field
                        :content-type
                        response))))
    (if (and content-type entity)
        (cond
          ((search "application/json" content-type)
           (make-json-entity-from-response response))
          ((search "text/html" content-type)
           (make-html-entity-from-response response))
          (t response))
        response)))

;; TODO: respect charset?
(defun make-json-entity-from-response (response)
  (change-class response 'json-entity)
  (let ((entity response))
    (let ((octets (read-response-body-into-vector response)))
      (with-slots (string json) entity
        (setf string (babel:octets-to-string octets)
              json (json:decode string))))
    entity))

;; TODO: respect charset
;; TODO: parse html
(defun make-html-entity-from-response (response)
  (change-class response 'html-entity)
  (let ((entity response))
    (let ((octets (read-response-body-into-vector response)))
      (with-slots (string html) entity
        (setf string (babel:octets-to-string octets)
              html string)))
    entity))

(defmacro with-connections (() &body body)
  `(let ()
     (unwind-protect
          (progn ,@body)
       )))
