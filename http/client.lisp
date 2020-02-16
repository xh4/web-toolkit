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
  `(let ((,response (read-response (connection-input-stream *connection*))))
     ,@body))

;; (with-connection ("http://example.com")
;;   (get "/")
;;   (receive (response)
;;     response))

(defun request (uri &key (method :get) header content)
  (let ((request (make-request uri method header content)))
    (let ((connection *connection*))
      (let ((output-stream (connection-output-stream connection))
            (input-stream (connection-input-stream connection)))
        (write-request output-stream request)
        (force-output output-stream)))))

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

#-lispworks
(defun open-connection (uri)
  (setf uri (process-connection-uri uri))
  (let ((https-p (equal "https" (uri-scheme uri))))
    (let ((host (uri-host uri))
          (port (or (uri-port uri) (if https-p 443 80))))
      (let ((socket (usocket:socket-connect host port
                                             :element-type '(unsigned-byte 8)
                                             #+sbcl :timeout #+sbcl 10
                                             :nodelay :if-supported)))
        (let ((connection (make-connection socket)))
          (with-slots (input-stream output-stream) connection
            (when https-p
              (setf output-stream (make-ssl-stream output-stream
                                                   :hostname host
                                                   :certificate nil
                                                   :key nil
                                                   :certificate-password nil
                                                   :verify nil
                                                   ;; :max-depth 10
                                                   :ca-file nil
                                                   :ca-directory nil)
                    input-stream output-stream)))
          connection)))))

#-lispworks
(defun make-ssl-stream (stream &key hostname certificate key certificate-password verify (max-depth 10) ca-file ca-directory)
  "Attaches SSL to the stream STREAM and returns the SSL stream
\(which will not be equal to STREAM)."
  (declare (ignorable stream certificate-password max-depth ca-directory hostname))
  (check-type verify (member nil :optional :required))
  (when (and certificate
             (not (probe-file certificate)))
    (error "certificate file ~A not found" certificate))
  (when (and key
             (not (probe-file key)))
    (error "key file ~A not found" key))
  (when (and ca-file
             (not (probe-file ca-file)))
    (error "ca file ~A not found" ca-file))
  (let ((ctx (cl+ssl:make-context :verify-depth max-depth
                                  :verify-mode cl+ssl:+ssl-verify-none+
                                  :verify-callback nil
                                  :verify-location (or (and ca-file ca-directory
                                                            (list ca-file ca-directory))
                                                       ca-file ca-directory
                                                       :default))))
    (cl+ssl:with-global-context (ctx)
      (cl+ssl:make-ssl-client-stream
       (cl+ssl:stream-fd stream)
       :verify verify
       :hostname hostname
       :close-callback (lambda ()
                         (close stream)
                         (cl+ssl:ssl-ctx-free ctx))
       :certificate certificate
       :key key
       :password certificate-password))))

#+lispworks
(defun open-connection (uri)
  (setf uri (process-connection-uri uri))
  (let ((https-p (equal "https" (uri-scheme uri))))
    (let ((host (uri-host uri))
          (port (or (uri-port uri) (if https-p 443 80))))
      (let ((socket (comm:connect-to-tcp-server host port :timeout 10)))
        (let ((connection (make-connection socket)))
          (let ((stream (connection-output-stream connection)))
            (setf (stream:stream-read-timeout stream) 10
                  (stream:stream-write-timeout stream) 10)
            (when https-p
              (comm:attach-ssl (connection-output-stream connection)
                               :ssl-ctx t
                               :ssl-side :client
                               :tlsext-host-name host)))
          connection)))))
