(in-package :http)

(defvar *connection* nil)

(defvar *connection-host* nil)

(defmacro with-connection ((uri) &body body)
  `(let* ((*connection* (open-connection ,uri))
          (*connection-host* (uri-host ,uri)))
     (unwind-protect
          (progn ,@body)
       (close-connection *connection*))))

(defmacro receive ((response) &body body)
  `(let ((,response (receive-response *connection*)))
     ,@body))

;; (with-connection ("http://example.com")
;;   (get "/")
;;   (receive (response)
;;     response))

(defun request (uri &key (method :get) header content)
  (let ((request (make-request uri method header content)))
    (send-request *connection* request)))

(defmacro define-request-method (symbol)
  `(defun ,symbol (uri)
     (request uri :method ,(make-keyword symbol))))

(define-request-method get)

(define-request-method post)

(define-request-method put)

(define-request-method delete)

(define-request-method head)

(defun make-request (uri method header content)
  (let ((request-uri (process-request-uri uri)))
    (let ((request-header (process-request-header header content)))
      (let ((request-body (process-request-content content)))
        (let ((request (make-instance 'request
                                      :method method
                                      :uri request-uri
                                      :version "HTTP/1.1"
                                      :header request-header
                                      :body request-body)))
          request)))))

(defun process-connection-uri (uri)
  (unless (uri-host uri) (error "Missing host in URI ~S" uri))
  (typecase uri
    (string uri)
    (uri (uri-string uri))))

(defun process-request-uri (uri)
  (let* ((uri (uri uri))
         (path (uri-path uri))
         (query (uri-query uri)))
    (concatenate 'string path (when query "?") query)))

(defun process-request-header (header content)
  (let ((new-header (make-instance 'header)))
    (set-header-field new-header (header-field "Host" *connection-host*))
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
