(in-package :websocket)

(define-constant +magic+
    "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=
  :documentation "Fixed magic WebSocket UUIDv4 key use in handshakes")

(define-condition websocket-error (simple-error)
  ((error-status :initarg :status :reader websocket-error-status))
  (:documentation "Superclass for all errors related to WebSocket."))

(defun websocket-error (status format-control &rest format-arguments)
  "Signals an error of type HUNCHENTOOT-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'websocket-error
         :status status
         :format-control format-control
         :format-arguments format-arguments))

(defmethod handle ((endpoint endpoint) (request request))
  (handler-bind ((error (lambda (c)
                          (invoke-debugger c))))
    (handle-endpoint-request endpoint request)))

(defun handle-endpoint-request (endpoint request)
  (let ((connection (header-field-value
                     (header-field request "Connection")))
        (upgrade (header-field-value
                  (header-field request "Upgrade"))))
    (when (and
           connection
           (member "upgrade" (cl-ppcre:split "\\s*,\\s*" connection)
                   :test #'string-equal)
           upgrade
           (string-equal "WebSocket" upgrade))
      (handle-handshake request *response*))))

(defun websocket-uri (path host &optional ssl)
  "Form WebSocket URL (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host path))

(defun handle-handshake (request response)
  (let ((requested-version (header-field-value
                            (header-field request "Sec-WebSocket-Version"))))
    (unless (equal "13" requested-version)
      (websocket-error 1002
                       "Unsupported websocket version ~a" requested-version)))
  (when (header-field request "Sec-WebSocket-Draft")
    (websocket-error 1002 "Websocket draft is unsupported"))
  (let* ((key (header-field-value
               (header-field request "Sec-WebSocket-Key")))
         (key+magic (concatenate 'string key +magic+)))
    (setf (header-field response "Sec-WebSocket-Accept")
          (base64:usb8-array-to-base64-string
           (ironclad:digest-sequence
            'ironclad:sha1
            (ironclad:ascii-string-to-byte-array
             key+magic)))))
  (let ((origin (header-field-value
                 (header-field request "Sec-WebSocket-Origin"))))
    (setf (header-field response "Sec-WebSocket-Origin") origin))
  (let* ((host (header-field-value
                (header-field request "Host")))
         (uri (request-uri request))
         (path (quri:uri-path (quri:uri uri))))
    (setf (header-field response "Sec-WebSocket-Location")
          (websocket-uri path host nil)))
  (let ((protocol (header-field-value
                   (header-field request "Sec-WebSocket-Protocol"))))
    (when protocol
      (setf (header-field response "Sec-WebSocket-Protocol")
            (first (cl-ppcre:split "\\s*,\\s*" protocol)))))
  (setf (response-status response) 101)
  (setf (header-field response "Upgrade") "WebSocket")
  (setf (header-field response "Connection") "Upgrade")
  (setf (header-field response "Content-Type") "application/octet-stream")

  (let ((stream (flex:make-flexi-stream (http::request-stream request)
                                        :external-format (flex:make-external-format :latin1 :eol-style :lf))))
    (format stream "HTTP/1.1 ~D ~A~C~C" 101 "Switching Protocols" #\Return #\Linefeed)
    (loop for header-field in (header-fields response)
       for name = (header-field-name header-field)
       for value = (header-field-value header-field)
       do (hunchentoot::write-header-line name value stream))
    (format stream "~C~C" #\Return #\Linefeed)
    (force-output stream)))
