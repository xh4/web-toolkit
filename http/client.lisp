(in-package :http)

(defun request (uri &key (method :get) header content)
  (check-request uri method header content)
  (let ((connection-manager (make-instance 'connection-manager)))
    (let ((request (make-request uri method header content)))
      (let ((connection (request-connection connection-manager request)))
        (send-request connection request)
        (let ((response (receive-response connection)))
          (release-connection connection-manager connection)
          (shutdown-connection-manager connection-manager)
          (process-response response request)
          response)))))

(defun check-request (uri method header content)
  (check-type uri (or uri:uri string))
  (unless (find method *methods*)
    (error "Request method ~A not supported" method))
  (when (and (find method '(:get :head :options)) content)
    (error "Request method ~A can't carry body" method))
  (check-type header (or header list)))

(defmacro define-request-method (symbol lambda-list)
  (let ((required-parameters (nth-value 0 (parse-ordinary-lambda-list lambda-list)))
        (keyword-parameters (nth-value 3 (parse-ordinary-lambda-list lambda-list))))
    `(defun ,symbol (,@lambda-list)
       (request ,(first required-parameters)
                :method ,(make-keyword symbol)
                ,@(when (= 2 (length required-parameters))
                    `(:content ,(second required-parameters)))
                ,@(loop for ((keyword-name name) nil nil) in keyword-parameters
                        append `(,keyword-name ,name))))))

(define-request-method get (uri &key header))

(defmacro with-get ((response uri &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (get ,uri :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(define-request-method head (uri &key header))

(defmacro with-head ((response uri &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (head ,uri :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(define-request-method put (uri content &key header))

(defmacro with-put ((response uri content &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (put ,uri ,content :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(define-request-method post (uri content &key header))

(defmacro with-post ((response uri content &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (post ,uri ,content :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(define-request-method delete (uri content &key header))

(defmacro with-delete ((response uri content &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (delete ,uri ,content :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(define-request-method patch (uri content &key header))

(defmacro with-patch ((response uri content &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (patch ,uri ,content :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(define-request-method options (uri &key header))

(defmacro with-options ((response uri &key header) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
           (let ((,response (options ,uri :header ,header)))
             (setf ,stream (response-body ,response))
             ,@body)
         (progn (when ,stream (ignore-errors (close ,stream))))))))

(defun make-request (uri method header content)
  (let ((uri (process-request-uri uri)))
    (let ((header (process-request-header header)))
      (unless (find-header-field "Host" header)
        (set-header-field header (header-field "Host" (uri-host uri))))
      (unless (find-header-field "Accept" header)
        (set-header-field header (header-field "Accept" "*/*")))
      (unless (find-header-field "User-Agent" header)
        (set-header-field header (header-field "User-Agent" "Lisp Web Toolkit")))
      (typecase content
        (string (make-text-entity content :header header :method method :uri uri :version "HTTP/1.1"))
        (json:object (make-json-entity content :header header :method method :uri uri :version "HTTP/1.1"))
        (pathname (make-file-entity content :header header :method method :uri uri :version "HTTP/1.1"))
        ((or null vector)
         (unless (find-header-field "Content-Length" header)
           (set-header-field header (header-field "Content-Length" (length content))))
         (make-instance 'request
                        :method method
                        :uri uri
                        :version "HTTP/1.1"
                        :header header
                        :body content))
        (t (error "Unable to make request using content of type ~A" (type-of content)))))))

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

(defun process-response (response request)
  (when (or (find (request-method request) '(:head :put))
            (not (message-body-present-p response)))
    (setf (response-body response) nil)
    (return-from process-response response))
  response)

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
(defun make-html-entity-from-response (response)
  (change-class response 'html-entity)
  (let ((entity response))
    (let ((octets (read-response-body-into-vector response)))
      (with-slots (string html) entity
        (setf string (babel:octets-to-string octets)
              html (html:parse string))))
    entity))

(defmacro with-connections (() &body body)
  `(let ()
     (unwind-protect
         (progn ,@body)
       )))
