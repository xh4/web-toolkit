(in-package :wt.http)

(defgeneric request-local-address (request)
  (:method ((request request))
    (pb:string-value (wt.proto.http:local-address request))))

(defgeneric request-local-port (request)
  (:method ((request request))
    (wt.proto.http:local-port request)))

(defgeneric request-remote-address (request)
  (:method ((request request))
    (pb:string-value (wt.proto.http:remote-address request))))

(defgeneric request-remote-port (request)
  (:method ((request request))
    (wt.proto.http:remote-port request)))

(defgeneric request-scheme (request)
  (:method ((request request))
    (case (wt.proto.http:scheme request)
      (0 :http)
      (1 :https))))

(defgeneric request-method (request)
  (:method ((request request))
    (case (wt.proto.http:method request)
      (0 :get)
      (1 :head)
      (2 :options)
      (3 :put)
      (4 :post)
      (5 :delete))))

(defgeneric request-path (request)
  (:method ((request request))
    (pb:string-value (wt.proto.http:path request))))

(defgeneric request-query (request)
  (:method ((request request))
    (pb:string-value (wt.proto.http:query request))))

(defgeneric request-version (request)
  (:method ((request request))
    (case (wt.proto.http:version request)
      (0 :http/1.0)
      (1 :http/1.1)
      (2 :http/2))))

(defgeneric request-headers (request)
  (:method ((request request))
    (loop for header across (wt.proto.http:headers request)
       collect
         (cons (pb:string-value (wt.proto.http:key header))
               (pb:string-value (wt.proto.http:value header))))))

(defgeneric request-body (request)
  (:method ((request request))
    (wt.proto.http:body request)))


;;; Helper functions


(defun request-query-parameters (request)
  (let ((query (request-query request)))
    (quri:url-decode-params query)))

(defun convert-hack (string external-format)
  "The rfc2388 package is buggy in that it operates on a character
stream and thus only accepts encodings which are 8 bit transparent.
In order to support different encodings for parameter values
submitted, we post process whatever string values the rfc2388 package
has returned."
  (flex:octets-to-string (map '(vector (unsigned-byte 8) *) 'char-code string)
                         :external-format external-format))

(defun parse-rfc2388-form-data (stream content-type-header external-format)
  "Creates an alist of POST parameters from the stream STREAM which is
supposed to be of content type 'multipart/form-data'."
  (let* ((parsed-content-type-header (rfc2388:parse-header content-type-header :value))
         (boundary (or (cdr (rfc2388:find-parameter
                             "BOUNDARY"
                             (rfc2388:header-parameters parsed-content-type-header)))
                       (return-from parse-rfc2388-form-data))))
    (loop for part in (rfc2388:parse-mime stream boundary :write-content-to-file t)
       for headers = (rfc2388:mime-part-headers part)
       for content-disposition-header = (rfc2388:find-content-disposition-header headers)
       for name = (cdr (rfc2388:find-parameter
                        "NAME"
                        (rfc2388:header-parameters content-disposition-header)))
       when name
       collect (cons (convert-hack name external-format)
                     (let ((contents (rfc2388:mime-part-contents part)))
                       (if (pathnamep contents)
                           (list contents
                                 (convert-hack (rfc2388:get-file-name headers) external-format)
                                 (rfc2388:content-type part :as-string t))
                           (convert-hack contents external-format)))))))

(defun parse-urlencoded-parameters (body)
  (quri:url-decode-params body))

(defun parse-multipart-parameters (content-type body)
  (let ((stream (flex:make-flexi-stream
                 (flex:make-in-memory-input-stream body))))
    (parse-rfc2388-form-data stream content-type :utf-8)))

(defun request-body-parameters (request)
  (let ((content-type (cdr (assoc "content-type" (request-headers request) :test #'string-equal)))
        (body (request-body request)))
    (when (> (length body) 0)
      (cond
        ((search "urlencoded" content-type) (parse-urlencoded-parameters body))
        ((search "multipart" content-type) (parse-multipart-parameters content-type body))
        (t (error "Don't know how to fetch body parameters of content type: ~S" content-type))))))
