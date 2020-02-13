(in-package :http-test)

(defmacro with-input-from-lines ((var lines &key (line-break http::+crlf+)) &body body)
  `(let* ((line-break (typecase ,line-break
                        (vector ,line-break)
                        (string (babel:string-to-octets ,line-break))
                        (character (babel:string-to-octets (string ,line-break)))))
          (line-octets (mapcar 'babel:string-to-octets ,lines))
          (all-octets (loop with all-octets = #()
                         for octets in line-octets
                         do (setf all-octets (concatenate 'vector all-octets octets)
                                  all-octets (concatenate 'vector all-octets line-break))
                         finally (return all-octets))))
     (babel-streams:with-input-from-sequence (,var all-octets)
       ,@body)))

(defun stream-length-p (n stream)
  (let ((content (alexandria::read-stream-content-into-byte-vector stream)))
    (= n (length content))))

(defun stream-empty-p (stream)
  (stream-length-p 0 stream))

(defclass test-socket ()
  ((open-p
    :initform t
    :accessor socket-open-p)))

(defmethod usocket:socket-close ((socket test-socket))
  (setf (socket-open-p socket) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar test-listener (listener :port 4004)))

(define-server test-server ()
  ()
  (:listener test-listener))

(defmacro with-request-in-stream ((stream request) &body body)
  `(let ((data (babel-streams:with-output-to-sequence (stream)
                 (loop for request in (ensure-list ,request)
                    do (http::write-request stream request)))))
     (let ((,stream (babel-streams:make-in-memory-input-stream data)))
       (unwind-protect
            ,@body
         (close ,stream)))))

(defmacro with-response-in-stream ((stream response) &body body)
  `(let ((data (babel-streams:with-output-to-sequence (stream)
                 (loop for response in (ensure-list ,response)
                    do (http::write-response stream response)))))
     (let ((,stream (babel-streams:make-in-memory-input-stream data)))
       (unwind-protect
            ,@body
         (close ,stream)))))

(defmacro with-output-to-string ((stream) &body body)
  `(babel:octets-to-string
    (babel-streams:with-output-to-sequence (,stream)
      ,@body)))

(defmacro with-read-header-field ((var line) &body body)
  `(with-input-from-lines (stream '(,line))
     (let ((,var (http::read-header-field stream)))
       (is-true (stream-empty-p stream))
       ,@body)))

(defmacro with-read-header ((var lines) &body body)
  `(with-input-from-lines (stream ,lines)
     (let ((,var (http::read-header stream)))
       ,@body)))

(defmacro with-connection ((connection &optional request) &body body)
  `(let ((socket (make-instance 'test-socket)))
     (with-request-in-stream (input-stream ,request)
       (let ((output-stream (babel-streams:make-in-memory-output-stream)))
         (let ((,connection (make-instance 'http::connection
                                           :listener test-listener
                                           :socket socket
                                           :input-stream input-stream
                                           :output-stream output-stream)))
           ,@body)))))

(defun process-request (request)
  (with-connection (connection (ensure-list request))
    (let ((cvar (bt:make-condition-variable))
          (lock (bt:make-lock)))
      (bt:acquire-lock lock)
      (let ((errorp))
        (let ((thread (bt:make-thread
                       (lambda ()
                         (handler-bind ((error (lambda (e)
                                                 (setf errorp t))))
                           (http::process-connection connection))
                         (bt:condition-notify cvar)))))
          (bt:condition-wait cvar lock :timeout 1)
          (unless errorp
            (ignore-errors
              (bt:destroy-thread thread))))))
    (let ((output-stream (http::connection-output-stream connection)))
      (let ((output-vector (babel-streams::vector-stream-vector
                            output-stream)))
        (let ((input-stream (babel-streams:make-in-memory-input-stream
                             output-vector)))
          (let ((response (loop for res = (http::read-response input-stream)
                              while res
                              do (http::read-response-body-into-vector res)
                              collect res)))
            response))))))

(defun map-pathnames (form)
  (let ((paths '()))
    (labels ((map-pathname (form &optional (base ""))
               (cond
                 ((and (listp form) (stringp (car form)))
                  (let ((name (car form)))
                    (unless (eq #\/ (aref name (1- (length name))))
                      (setf name (concatenate 'string name "/")))
                    (let ((path (merge-pathnames name base)))
                      (appendf paths (list path))
                      (map-pathname (cdr form) path))))
                 ((stringp form)
                  (let ((name form))
                    (let ((path (merge-pathnames name base)))
                      (appendf paths (list path)))))
                 ((listp form)
                  (loop for f in form do (map-pathname f base))))))
      (map-pathname form))
    paths))

(defmacro with-static-files ((root &optional fd-form) &body body)
  (let ((pathnames (map-pathnames fd-form)))
    (let ((root-pathname (merge-pathnames
                 (format nil "~A/" (symbol-name (gensym "WT-STATIC-")))
                 uiop:*temporary-directory*)))
      `(let ((,root ,root-pathname))
         (ensure-directories-exist ,root)
         (loop for part in ',pathnames
            for pathname = (merge-pathnames part ,root)
            if (http::directory-pathname-p pathname)
            do (ensure-directories-exist pathname)
            else
            do (with-open-file (stream pathname :direction :output :if-does-not-exist :create)))
         (unwind-protect
              (progn ,@body)
           (uiop:delete-directory-tree ,root :validate t))))))
