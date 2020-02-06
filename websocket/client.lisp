(in-package :websocket)

(defun connect (endpoint uri)
  (unless (equal (uri-scheme uri) "ws")
    (error "Expect ws scheme in uri"))
  (let* ((host (uri-host uri))
         (port (or (uri-port uri) 80))
         (request (make-instance 'http:request
                                 :method :get
                                 :uri uri))
         (stream (open-tcp-stream host port)))
    (unless host
      (error "Missing host in uri"))
    (let ((request-header (make-instance 'header))
          (request-key (make-handshake-key)))
      (flet ((set-header-field (name value)
               (http::set-header-field
                request-header
                (header-field name value))))
        (set-header-field "Host" (format nil "~A~@[:~A~]" host port))
        (set-header-field "Upgrade" "WebSocket")
        (set-header-field "Connection" "Upgrade")
        (set-header-field "Sec-WebSocket-Key" request-key)
        (set-header-field "Sec-WebSocket-Version" "13"))
      (setf (request-header request) request-header)
      (http::write-request stream request)
      (force-output stream)
      (let ((status-line (http::read-status-line stream)))
        (destructuring-bind (http-version status-code reason-phase) status-line
          (declare (ignore http-version reason-phase))
          (unless (= 101 status-code)
            (close stream)
            (error "Unexpected response code ~D" status-code))
          (let ((response-header (http::read-header stream)))
            (let ((response-accept (header-field-value
                                    (find-header-field "Sec-WebSocket-Accept" response-header)))
                  (expected-response-accept (base64:usb8-array-to-base64-string
                                             (ironclad:digest-sequence
                                              'ironclad:sha1
                                              (ironclad:ascii-string-to-byte-array
                                               (concatenate 'string
                                                            request-key
                                                            +magic+))))))
              (unless (equal response-accept expected-response-accept)
                (close stream)
                (error "Mismatch key and accept")))
            (let ((response (make-instance 'http:response
                                           :status status-code
                                           :header response-header)))
              (let ((connection (make-instance 'connection
                                               :state :open
                                               :handshake-request request
                                               :handshake-response response
                                               :input-stream stream
                                               :output-stream stream
                                               :mask-frame-p t)))
                (let ((session-class (decide-endpoint-session-class endpoint request)))
                  (let ((session (make-session-instance session-class connection request)))
                    (invoke-open-handler endpoint session)
                    (bt:make-thread
                     (lambda ()
                       (process-connection endpoint session connection))
                     :initial-bindings `((*standard-output* . ,*standard-output*)
                                         (*error-output* . ,*error-output*)))
                    session))))))))))

(defun open-tcp-stream (host port)
  #+:lispworks
  (comm:open-tcp-stream host port
                        :element-type '(unsigned-byte 8)
                        :timeout 10
                        :read-timeout 10
                        #-:lw-does-not-have-write-timeout
                        :write-timeout
                        #-:lw-does-not-have-write-timeout
                        10
                        :errorp t)
  #-:lispworks
  (usocket:socket-stream
   (usocket:socket-connect host port
                           :element-type '(unsigned-byte 8)
                           #+:openmcl :deadline
                           #+:openmcl deadline
                           #+(or abcl clisp lispworks mcl openmcl sbcl)
                           :timeout
                           #+(or abcl clisp lispworks mcl openmcl sbcl)
                           connection-timeout
                           :nodelay :if-supported)))

(defun make-handshake-key ()
  (let ((key (with-output-to-string (stream)
               (dotimes (i 16)
                 (write-char (code-char (random 255)) stream)))))
    (base64:string-to-base64-string key)))
