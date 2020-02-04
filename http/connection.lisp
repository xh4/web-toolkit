(in-package :http)

(defclass connection ()
  ((socket
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
    :accessor connection-output-stream)))

(defun make-and-process-connection (listener socket)
  (let ((connection (make-connection socket)))
    (bt:make-thread
     (lambda ()
       (process-connection listener connection))
     :initial-bindings `((*standard-output* . ,*standard-output*)
                         (*error-output* . ,*error-output*)))
    connection))

(defun make-connection (socket)
  (let ((stream (usocket:socket-stream socket)))
    (make-instance 'connection
                   :socket socket
                   :input-stream stream
                   :output-stream stream)))

(defun process-connection (listener connection)
  (with-slots (input-stream output-stream) connection
    (tagbody :start
       (if-let ((request (read-request input-stream)))
         (let ((response (handle-request listener connection request)))
           (if (= 101 (status-code (response-status response)))
               (go :end)
               (progn
                 (handle-response listener connection response)
                 (if (or (search "close" (header-field-value
                                          (find-header-field request "Connection")))
                         (search "close" (header-field-value
                                          (find-header-field response "Connection")))
                         (= 101 (status-code (response-status response)))
                         (not (open-stream-p input-stream)))
                     (go :end)
                     (go :start))))))
     :end (close-connection connection))))

(defun close-connection (connection)
  (with-slots (socket input-stream output-stream) connection
    (close input-stream)
    (close output-stream)
    (usocket:socket-close socket)))

(defun handle-request (listener connection request)
  (let ((server (listener-server listener)))
    (let ((handler (server-handler server)))
      (unless handler
        (setf handler default-handler))
      (invoke-handler handler request))))

(defun handle-response (listener connection response)
  (let ((stream (connection-output-stream connection)))
    (write-response stream response)
    (force-output stream)))

(defun handle-pathname-response (response)
  (let ((pathname (response-body response)))

    (when (or (wild-pathname-p pathname)
              (not (fad:file-exists-p pathname))
              (fad:directory-exists-p pathname))
      ;; Missing
      (setf (response-status response) 404)
      (add-header-field (response-header response)
                        (header-field "Content-Type" "text/plain"))
      (setf (response-body response) "static handler: not found")
      (return-from handle-pathname-response))

    ;; Content-Type
    (let ((content-type (or (header-field-value
                             (header-field response "Content-Type"))
                            (mime-type pathname)
                            "application/octet-stream")))
      ;; Charset
      (if (and (cl-ppcre:scan "(?i)^text" content-type)
               (not (cl-ppcre:scan "(?i);\\s*charset=" content-type)))
          (setf content-type
                (format nil "~A; charset=~(~A~)" content-type
                        (flex:external-format-name
                         (flex:make-external-format :utf8 :eol-style :lf)))))
      (add-header-field (response-header response)
                        (header-field "Content-Type" content-type)))

    ;; Last-Modified
    (let ((time (or (file-write-date pathname)
                    (get-universal-time))))
      (add-header-field (response-header response)
                        (header-field "Last-Modified" (rfc-1123-date time))))

    (with-open-file (input-stream pathname
                                  :direction :input
                                  :element-type '(unsigned-byte 8))
      (let* ((bytes-to-send (file-length input-stream))
             (buffer-size 8192)
             (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
        (let ((output-stream (handle-response-header response)))
          (loop
             (when (zerop bytes-to-send)
               (return))
             (let* ((chunk-size (min buffer-size bytes-to-send)))
               (unless (eql chunk-size (read-sequence buffer input-stream
                                                      :end chunk-size))
                 (error "can't read from input file"))
               (write-sequence buffer output-stream :end chunk-size)
               (decf bytes-to-send chunk-size)))
          (finish-output output-stream))))))
