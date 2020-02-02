(in-package :http)

(defclass request ()
  ((method
    :initarg :method
    :initform nil
    :accessor request-method)
   (uri
    :initarg :uri
    :initform nil
    :accessor request-uri)
   (header
    :initarg :header
    :initform nil
    :accessor request-header)
   (body
    :initarg :body
    :initform nil
    :accessor request-body)))

(defgeneric request-method (request))

(defgeneric (setf request-method) (value request))

;; scheme + host + port + path + query
(defgeneric request-uri (request))

(defgeneric (setf request-uri) (value request))

(defgeneric request-version (request))

(defgeneric (setf request-version) (value request))

(defgeneric request-header (request))

(defgeneric (setf request-header) (header request))

(defgeneric request-body (request))

(defgeneric (setf request-body) (body request))

(defmethod header-fields ((request request))
  (let ((header (request-header request)))
    (header-fields header)))

(defmethod (setf header-fields) (value (request request))
  (let ((header (request-header request)))
    (setf (header-fields header) value)))

(defmethod find-header-field ((request request) name)
  (find-header-field (request-header request) name))

(defparameter *methods* '(:get :post :put :delete :head))

(defvar *request-stream-mapping-table*
  (trivial-garbage:make-weak-hash-table :weakness :key))

(defun request-stream (request)
  (gethash request *request-stream-mapping-table*))


(defun read-request-line (stream)
  (let ((line (handler-case
                  (read-line stream)
                ((or end-of-file #-:lispworks usocket:timeout-error) nil))))
    (parse-request-line line)))

(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))

(defun parse-request-line (line)
  (unless (every #'printable-ascii-char-p line)
    (error "Non-ASCII character in request line"))
  (cl-ppcre:split "\\s+" line :limit 3))

(defun write-request-line (stream method request-uri http-version)
  (check-type method (or string symbol))
  (check-type request-uri string)
  (check-type http-version string)
  (when (symbolp method)
    (setf method (symbol-name method)))
  (let ((line (format nil "~A ~A ~A" method request-uri http-version)))
    (write-string line stream)
    (write-sequence +crlf+ stream)
    (+ (length line) (length +crlf+))))

(defun write-request-body (stream body)
  (write-sequence body stream))

(defun read-request (stream)
  )

(defgeneric write-request (stream request)
  (:method (stream (request request))
    (let ((method (request-method request))
          (uri (request-uri request))
          (header (request-header request))
          (body (request-body request)))
      (let ((request-uri (or (uri-path uri) "/")))
        (when-let ((query (uri-query uri :decode nil)))
          (setf request-uri (concatenate 'string request-uri "?" query)))
        (write-request-line stream method request-uri "HTTP/1.1")
        (write-header stream header)
        (write-request-body stream body)))))
