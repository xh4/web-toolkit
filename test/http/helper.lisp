(in-package :http-test)

(defvar *it* nil)
(defvar *is* nil)

(defmacro => (n form)
  `(let ((*is* ',(append `(=> ,n) (list form))))
     ,form))

(defmacro it (&body body)
  (setf body
        (let ((n 1))
          (serapeum:map-tree
           (lambda (form)
             (if (and (listp form)
                      (not (emptyp form))
                      (member (first form) '(fiveam:is fiveam:is-true
                                             fiveam:is-every fiveam:is-false
                                             fiveam:signals fiveam:finishes
                                             fiveam:pass fiveam:fail)))
                 (throw :skip (prog1
                                  (append `(=> ,n) (list form))
                                (incf n)))
                 form))
           body
           :skip)))
  `(let ((*it* ',body))
     ,@body))

(defun fiveam::process-failure (test-expr &optional reason-format &rest format-args)
  (let ((reason (and reason-format
                     (apply #'format nil reason-format format-args)))
        (premble))
    (setf *it*
          (serapeum:map-tree
           (lambda (form)
             (if (and (listp form)
                      (not (emptyp form))
                      (equal (first form) '=>))
                 (throw :skip
                   (if (equal (second *is*) (second form))
                       (cons '=> (cddr form))
                       (cddr form)))
                 form))
           *it*
           :skip))
    (loop for form in *it*
       do (setf premble (concatenate 'string
                                     premble
                                     (format nil "~%IN ~%~A~%"
                                             (cl:with-output-to-string (stream)
                                               (pprint form stream))))))
    (setf reason (concatenate 'string premble reason))
    (with-simple-restart (fiveam::ignore-failure "Continue the test run.")
      (error 'fiveam::check-failure :test-expr test-expr
             :reason reason))
    (fiveam::add-result 'fiveam::test-failure :test-expr test-expr
                        :reason reason)))

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

(defmacro with-process-connection ((request response) &body body)
  `(with-connection (connection ,request)
     (let ((cvar (bt:make-condition-variable))
           (lock (bt:make-lock)))
       (bt:acquire-lock lock)
       (let ((thread (bt:make-thread
                      (lambda ()
                        (http::process-connection connection)
                        (bt:condition-notify cvar)))))
         (bt:condition-wait cvar lock :timeout 5)
         (bt:destroy-thread thread)))
     (let ((output-stream (http::connection-output-stream connection)))
       (let ((output-vector (babel-streams::vector-stream-vector
                             output-stream)))
         (let ((input-stream (babel-streams:make-in-memory-input-stream
                              output-vector)))
           (let ((,response (loop for res = (http::read-response input-stream)
                               while res
                               do (http::read-response-body-into-vector res)
                               collect res)))
             ,@body))))))

;; (with-process-connection ((list (make-instance 'request
;;                                                :method "GET"
;;                                                :uri "/"
;;                                                :version "HTTP/1.0"
;;                                                :header (header "Connection" "close")))
;;                           response)
;;   response)
