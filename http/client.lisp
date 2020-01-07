(in-package :http)

(defun process-request-uri (uri)
  (typecase uri
    (uri (setf uri (uri-string uri)))
    (string uri)))

(defun process-request-header (header &key content-type
                                        referer user-agent range
                                        accept basic-authorization)
  (let ((header (typecase header
                  (header (loop for header-field in (header-fields header)
                             collect (cons (header-field-name header-field)
                                           (header-field-value header-field))))
                  (list header))))
    (when content-type
      (add-header-field header (header-field "Content-Type" content-type)))
    (when referer
      (add-header-field header (header-field "Referer" referer)))
    (when user-agent
      (add-header-field header (header-field "User-Agent" user-agent)))
    (when range
      (add-header-field header (header-field "Range" range)))
    (when accept
      (add-header-field header (header-field "Accept" accept)))
    (when basic-authorization
      (add-header-field header (header-field "..." accept)))
    header))

(defun request (uri &key (method :get) header body (redirect 5)
                      content-type referer user-agent range accept
                      basic-authorization)

  (let ((request-uri (process-request-uri uri)))
    (let ((request-header (process-request-header
                           header
                           :content-type content-type
                           :referer referer
                           :user-agent user-agent
                           :range range
                           :accept accept
                           :basic-authorization basic-authorization)))
      (multiple-value-bind (stream status-code response-headers)
          (drakma:http-request request-uri
                               :additional-headers request-header
                               :method method
                               :redirect redirect
                               :content body
                               :want-stream t)
        (let ((response-header (make-instance 'header))
              (response-status (status status-code)))
          (loop for (name . value) in response-headers
             do
               (add-header-field response-header (header-field name value)))
          (let ((response (make-instance 'response
                                         :status response-status
                                         :header response-header
                                         :body stream)))
            response))))))

(defmacro define-http-request-method (symbol)
  `(defun ,symbol (uri)
     (request uri :method ,(make-keyword symbol))))

(define-http-request-method get)

(define-http-request-method post)

(define-http-request-method put)

(define-http-request-method delete)

(define-http-request-method head)
