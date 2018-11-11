(in-package :wt)

(defun fetch-query-parameters (request)
  "Return an alist of parameters parsed from query string.
The names of parameters are case-sensitive strings."
  (when-let ((query (getf request :query)))
    (quri:url-decode-params query)))

(defun fetch-body-parameters (request)
  "Return an alist of parameters parsed from request's body.
The names of parameters are case-sensitive strings.
Note that this function may be called only once on a request depending on
the request's content type."
  (flet ((fetch-urlencoded-parameters ()
           (let* ((body-input-stream (getf request :body))
                  (body-string (alexandria::read-stream-content-into-string body-input-stream)))
             (quri:url-decode-params body-string)))
         (fetch-multipart-parameters ()
           (let ((content-type (getf request :content-type))
                 (body-input-stream (getf request :body)))
             (hunchentoot::parse-rfc2388-form-data body-input-stream content-type :utf-8))))
    (let ((content-type (getf request :content-type)))
      (cond
        ((search "urlencoded" content-type) (fetch-urlencoded-parameters))
        ((search "multipart" content-type) (fetch-multipart-parameters))
        (t (error "Don't know how to fetch body parameters of content type: ~S" content-type))))))

(defun fetch-parameters (request)
  "Return an alist of parameters.
The names of parameters are maked into keywords.
Note that this function may be called only once on a request depending on
the request's content type."
  (let ((parameters (append (fetch-query-parameters request)
                            (fetch-body-parameters request))))
    (loop for (name . value) in parameters
         collect `(,(make-keyword (string-upcase (param-case name))) . ,value))))
