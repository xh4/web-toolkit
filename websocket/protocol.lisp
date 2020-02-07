(in-package :websocket)

(define-constant +magic+
    "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=
  :documentation "Fixed magic WebSocket UUIDv4 key use in handshakes")

(define-condition websocket-error (simple-error)
  ((code :initarg :code :reader websocket-error-code))
  (:documentation "Superclass for all errors related to WebSocket."))

(defun websocket-error (code format-control &rest format-arguments)
  (error 'websocket-error
         :code code
         :format-control format-control
         :format-arguments format-arguments))

(define-condition text-received ()
  ((text
    :initarg :text
    :initform nil)))

(define-condition binary-received ()
  ((data
    :initarg :data
    :initform nil)))

(define-condition close-received ()
  ((code
    :initarg :code
    :initform nil)
   (reason
    :initarg :reason
    :initform nil)))

(defun symbol-function-p (symbol)
  (if (and (fboundp symbol)
           (not (macro-function symbol))
           (not (special-operator-p symbol)))
      (symbol-function symbol)
      nil))

;; TODO: raise error when session-class is a symbol but the class not found
(defun decide-endpoint-session-class (endpoint request)
  (let ((object (endpoint-session-class endpoint)))
    (typecase object
      (null 'session)
      (symbol (cond
                ((find-class object nil) object)
                ((symbol-function-p object)
                 (let ((value (funcall object request)))
                   (typecase value
                     (symbol value)
                     (t (error "Expect symbol")))))))
      (function (let ((value (funcall object request)))
                  (typecase value
                    (symbol value)
                    (t (error "Expect symbol"))))))))

(defun make-session-instance (session-class connection request)
  (make-instance session-class
                 :connection connection
                 :opening-uri (request-uri request)
                 :opening-header (request-header request)))

(defun handle-endpoint-request (endpoint request)
  (handler-bind ((error (lambda (c)
                          (trivial-backtrace:print-backtrace c))))
    (let ((response (handle-handshake request)))
      (let* ((stream (request-body request))
             (connection (make-instance 'connection
                                        :state :open
                                        :handshake-request request
                                        :handshake-response response
                                        :input-stream stream
                                        :output-stream stream)))
        (let ((session-class (decide-endpoint-session-class endpoint request)))
          (let ((session (make-session-instance session-class connection request)))

            (let ((result (invoke-open-handler endpoint session)))
              (when (typep result 'session)
                (setf session result)))

            ;; Remove timeouts before process connection
            #+lispworks
            (setf (stream:stream-read-timeout stream) nil
                  (stream:stream-write-timeout stream) nil)

            (process-connection endpoint session connection))))
      response)))

(defun process-connection (endpoint session connection)
  (block nil
    (handler-bind ((websocket-error
                    (lambda (error)
                      (invoke-error-handler endpoint session error)
                      (return)))
                   (babel-encodings:character-decoding-error
                    (lambda (error)
                      (close-connection connection
                                        :code 1007
                                        :reason "Bad UTF-8")
                      (invoke-error-handler endpoint session error)
                      (return)))
                   (error
                    (lambda (error)
                      (close-connection connection
                                        :code 1011
                                        :reason "Internal error")
                      (invoke-error-handler endpoint session error)
                      (return)))

                   (text-received
                    (lambda (c)
                      (let ((message (slot-value c 'text)))
                        (invoke-message-handler endpoint session message))))

                   (binary-received
                    (lambda (c)
                      (let ((message (slot-value c 'data)))
                        (invoke-message-handler endpoint session message))))

                   (close-received
                    (lambda (c)
                      (let ((code (slot-value c 'code))
                            (reason (slot-value c 'reason)))
                        (invoke-close-handler endpoint session code reason)))))
      (with-slots (state) connection
        (loop for frame = (handler-bind ((error (lambda (e)
                                                  (drop-connection connection)
                                                  (invoke-error-handler endpoint session e)
                                                  (invoke-close-handler endpoint session 1001 "Peer closed unexpectedly")
                                                  (return))))
                            (receive-frame connection))
           do (handle-frame connection frame)
           while (not (or (eq state :closed)
                          (eq state :closing))))))))

(defun handle-handshake (request)
  (check-handshake-request request)
  (accept-handshake-request request)
  (reply
   (status :switching-protocols)
   (header :upgrade "WebSocket")
   (header :connection "Upgrade"))
  (let ((stream (request-body request)))
    (http::write-status-line stream "HTTP/1.1" 101 "Switching Protocols")
    (http::write-header stream (response-header *response*))
    (force-output stream))
  *response*)

(defun check-handshake-request (request)
  (unless (and
           (string-equal "Upgrade" (header-field-value
                                    (find-header-field "Connection" request)))
           (string-equal "WebSocket" (header-field-value
                                      (find-header-field "Upgrade" request)))
           (string-equal "13" (header-field-value
                               (find-header-field "Sec-WebSocket-Version" request))))
    (error "Bad WebSocket request")))

(defun accept-handshake-request (request)
  (let ((key (header-field-value
              (find-header-field "Sec-WebSocket-Key" request))))
    (let ((accept (base64:usb8-array-to-base64-string
                   (ironclad:digest-sequence
                    'ironclad:sha1
                    (ironclad:ascii-string-to-byte-array
                     (concatenate 'string key +magic+))))))
      (reply (header-field "Sec-WebSocket-Accept" accept)))))
