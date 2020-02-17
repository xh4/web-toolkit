(in-package :http)

(defclass connection-manager ()
  ((connections
    :initarg :connections
    :initform nil
    :accessor connection-manager-connections)))

(defgeneric request-connection (connection-manager request)
  (:method (connection-manager request)
    (let ((uri (request-uri request)))
      (let ((connection (open-connection uri)))
        (with-slots (connections) connection-manager
          (push connection connections))
        connection))))

(defgeneric release-connection (connection-manager connection)
  (:method (connection-manager connection)
    (with-slots (connections) connection-manager
      (setf connections (remove connection connections)))))

(defgeneric shutdown-connection-manager (connection-manager)
  (:method (connection-manager)
    (with-slots (connections) connection-manager
      (loop for connection in connections
         do (close-connection connection))
      (setf connections nil))))

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
