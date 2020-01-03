(in-package :uri)

(defgeneric render-uri (uri &key encode)
  (:method ((uri uri) &key (encode t))
    (when (null (uri-string uri))
      (setf (uri-string uri)
            (let ((scheme (uri-scheme uri))
                  (host (uri-host uri))
                  (userinfo (uri-userinfo uri))
                  (port (uri-port uri))
                  (path (uri-path uri))
                  (query (uri-query uri))
                  (fragment (uri-fragment uri)))
              (concatenate 'string
                           (when scheme
                             scheme)
                           (when scheme ":")
                           (when host
                             "//")
                           (when userinfo
                             userinfo)
                           (when userinfo
                             "@")
                           (when host
                             host)
                           (when port
                             ":")
                           (when port
                             (format nil "~A" port))
                           (when path
                             path)
                           (when query
                             "?")
                           (when query
                             query)
                           (when fragment
                             "#")
                           (when fragment
                             fragment)))))
    (uri-string uri)))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity t)
    (format stream "~S" (render-uri uri))))
