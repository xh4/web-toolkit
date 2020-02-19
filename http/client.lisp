(in-package :http)

(defun request (uri &key (method :get) header content)
  (check-request uri method header content)
  (let ((connection-manager (make-instance 'connection-manager)))
    (let ((request (make-request uri method header content)))
      (let ((connection (request-connection connection-manager request)))
        (send-request connection request)
        (let ((response (receive-response connection)))
          (process-response response request)
          response)))))

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

(define-request-method get (uri &key header))

(define-request-method head (uri &key header))

(define-request-method put (uri &key header content))

(define-request-method post (uri &key header content))

(define-request-method delete (uri &key header content))

(define-request-method patch (uri &key header content))

(define-request-method options (uri &key header))

(defun make-request (uri method header content)
  (let ((uri (process-request-uri uri)))
    (let ((header (process-request-header header content)))
      (unless (find-header-field "Host" header)
        (set-header-field header (header-field "Host" (uri-host uri))))
      (let ((body (process-request-content content)))
        (let ((request (make-instance 'request
                                      :method method
                                      :uri uri
                                      :version "HTTP/1.1"
                                      :header header
                                      :body body)))
          request)))))

(defun process-request-uri (uri)
  (unless (uri-host uri) (error "Missing host in URI ~S" uri))
  (typecase uri
    (string uri)
    (uri (uri-string uri))))

(defun process-request-header (header content)
  (let ((new-header (make-instance 'header)))
    ;; TODO: forbid user from setting some header fields
    (typecase header
      (header (loop for header-field in (header-fields header)
                 do (set-header-field new-header header-field)))
      (list (when header
              (loop for (name . value) in header
                 do (set-header-field new-header (header-field name value))))))
    new-header))

(defun process-request-content (content)
  )

(defun process-response (response request)
  (when (find (request-method request) '(:head :put))
    (setf (response-body response) nil))
  ;; TODO: set body to nil if content-length is 0)
  response)

(defmacro with-connections (() &body body)
  `(let ()
     (unwind-protect
          (progn ,@body)
       )))
