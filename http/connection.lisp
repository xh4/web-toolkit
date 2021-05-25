(in-package :http)

(defclass connection ()
  ((id
    :initarg :id
    :initform 0
    :accessor connection-id)
   (listener
    :initarg :listener
    :initform nil
    :accessor connection-listener)
   (socket
    :initarg :socket
    :initform nil
    :accessor connection-socket)
   (input-stream
    :initarg :input-stream
    :initform nil
    :accessor connection-input-stream)
   (output-stream
    :initarg :output-stream
    :initform nil
    :accessor connection-output-stream)
   (local-port
    :initarg :local-port
    :initform nil
    :accessor connection-local-port)
   (local-address
    :initarg :local-address
    :initform nil
    :accessor connection-local-address)
   (peer-port
    :initarg :peer-port
    :initform nil
    :accessor connection-peer-port)
   (peer-address
    :initarg :peer-address
    :initform nil
    :accessor connection-peer-address)
   (last-request
    :initarg :last-request
    :initform nil
    :accessor connection-last-request)
   (open-time
    :initform (local-time:now)
    :accessor connection-open-time)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t :identity t)
    (format stream "[~A:~A - ~A:~A] "
            (connection-peer-address connection)
            (connection-peer-port connection)
            (connection-local-address connection)
            (connection-local-port connection))
    (when-let* ((pointer (connection-last-request connection))
                (request (weak-pointer-value pointer)))
      (format stream "~A" (request-method request))
      (format stream " ~A" (request-uri request)))))

(defvar *counter* 0)
(defun next-connection-id ()
    #+allegro
    (excl:incf-atomic *counter*)
    #+lispworks
    (system:atomic-incf *counter*)
    #+sbcl
    (sb-ext:atomic-incf *counter*)
    #-(or allegro lispworks sbcl)
    (incf *counter*))

(defvar *connection* nil)

(defvar *request-connection-mapping* (make-weak-hash-table :weakness :key))

(defun make-and-process-connection (listener socket)
  (let ((connection (make-connection socket listener)))
    (bt:make-thread
     (lambda ()
       (process-connection connection))
     :name (format nil "Connection on ~A" (listener-port listener))
     :initial-bindings `((*standard-output* . ,*standard-output*)
                         (*error-output* . ,*error-output*)
                         (*connection* . ,connection)))
    connection))

#-lispworks
(defun make-connection (socket &optional listener)
  (let ((stream (usocket:socket-stream socket)))
    (let ((local-port (usocket:get-local-port socket))
          (local-address (usocket:get-local-address socket))
          (peer-port (usocket:get-peer-port socket))
          (peer-address (usocket:get-peer-address socket)))
      (flet ((format-ip-address (address)
               (unless (= 4 (length address))
                 (error "Expect IPv4 address, got ~A" address))
               (format nil "~{~A~,^.~}" (coerce address 'list))))
        (let ((connection (make-instance 'connection
                                         :id (next-connection-id)
                                         :listener listener
                                         :socket socket
                                         :input-stream stream
                                         :output-stream stream
                                         :local-port local-port
                                         :local-address (format-ip-address local-address)
                                         :peer-port peer-port
                                         :peer-address (format-ip-address peer-address))))
          connection)))))

#+lispworks
(defun make-connection (socket &optional listener)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket socket
                               :direction :io
                               :element-type '(unsigned-byte 8))))
    (setf (stream:stream-read-timeout stream) 10
          (stream:stream-write-timeout stream) 10)
    (let ((connection (make-instance 'connection
                                     :listener listener
                                     :socket socket
                                     :input-stream stream
                                     :output-stream stream)))
      (multiple-value-bind (address port) (comm:socket-stream-address stream)
        (setf (connection-local-port connection) port
              (connection-local-address connection) (when address
                                                      (comm:ip-address-string address))))
      (multiple-value-bind (address port) (comm:socket-stream-peer-address stream)
        (setf (connection-peer-port connection) port
              (connection-peer-address connection) (when address
                                                     (comm:ip-address-string address))))
      connection)))

(defun process-connection (connection)
  (tagbody :start
     ;; TODO: handle errors here
     (handler-bind ((error (lambda (error)
                             (declare (ignore error))
                             (go :end))))
       (when-let ((request (receive-request connection)))
         (setf (gethash request *request-connection-mapping*) connection)
         (setf (connection-last-request connection) (make-weak-pointer request))
         (let ((response (if (string-equal "Upgrade"
                                           (header-field-value
                                            (find-header-field
                                             "Connection"
                                             request)))
                             (handle-request connection request)
                             #+allegro
                             (mp:with-timeout (300 (error "Handle timeout"))
                               (handle-request connection request))
                             #-allegro (handle-request connection request))))
           (if (equal 101 (status-code (response-status response)))
               (go :end)
               (progn
                 (handle-response connection response)
                 (if (connection-keep-alive-p connection request response)
                     (progn
                       (let ((body (request-body request)))
                         (typecase body
                           (stream (stream-read-remain body))))
                       (go :start))
                     (go :end)))))))
   :end (handler-case
            (close-connection connection)
          (error (e)
            ;; TODO: handler error
            (declare (ignore e))))))

#-lispworks
(defun close-connection (connection)
  (with-slots (socket) connection
    (usocket:socket-close socket)))

#+lispworks
(defun close-connection (connection)
  (with-slots (socket) connection
    (close input-stream)
    (close output-stream)))

(defgeneric send-request (connection request)
  (:method ((connection connection) request)
    (let ((output-stream (connection-output-stream connection)))
      (prog1
          (write-request output-stream request)
        (force-output output-stream)))))

(defgeneric receive-request (connection)
  (:method ((connection connection))
    (let ((input-stream (connection-input-stream connection)))
      (read-request input-stream))))

(defgeneric send-response (connection response)
  (:method ((connection connection) response)
    (let ((output-stream (connection-output-stream connection)))
      (prog1
          (write-response output-stream response)
        (force-output output-stream)))))

(defgeneric receive-response (connection)
  (:method ((connection connection))
    (let ((input-stream (connection-input-stream connection)))
      (read-response input-stream))))

(defun handle-request (connection request)
  (let ((listener (connection-listener connection)))
    (let ((server (listener-server listener)))
      (let ((handler (server-handler server)))
        (unless handler
          (setf handler default-handler))
        (let ((response (invoke-handler handler request)))
          (when (and (equal "HTTP/1.0" (request-version request))
                     (string-equal "Keep-Alive" (header-field-value
                                                 (find-header-field "Connection" request))))
            (set-header-field response (header-field "Connection" "Keep-Alive")))
          response)))))

(defun handle-response (connection response)
  (when (null (response-status response))
    (setf response (make-instance 'response
                                  :status 200
                                  :header (header :content-length 0))))
  (unless (equal 101 (status-code (response-status response)))
    (prog1
        (set-header-field response (header-field "Date" (rfc-1123-date)))
      (send-response connection response))))

(defgeneric keep-alive-p (object)
  (:method ((request request))
    (let ((header (request-header request)))
      (or (and (equal "HTTP/1.0" (request-version request))
               (string-equal "Keep-Alive" (header-field-value
                                           (find-header-field "Connection" header))))
          (and (equal "HTTP/1.1" (request-version request))
               (not (string-equal "Close" (header-field-value
                                           (find-header-field "Connection" header))))))))
  (:method ((response response))
    (let ((header (response-header response)))
      (not (string-equal "Close" (header-field-value
                                  (find-header-field "Connection" header))))))
  (:method ((entity entity))
    (let ((header (response-header entity)))
      (not (string-equal "Close" (header-field-value
                                  (find-header-field "Connection" header)))))))

(defun connection-keep-alive-p (connection request response)
  (and (keep-alive-p request)
       (keep-alive-p response)
       (not (equal 101 (status-code (response-status response))))
       (open-stream-p (connection-input-stream connection))
       (open-stream-p (connection-output-stream connection))))

(defgeneric connection (object)
  (:method ((request request))
    (gethash request *request-connection-mapping*)))
