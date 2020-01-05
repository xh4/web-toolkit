(in-package :uri)

(defgeneric render-uri (uri)
  (:method ((uri uri))
    (when (null (uri-string uri))
      (let ((scheme (uri-scheme uri))
            (host (uri-host uri))
            (userinfo (uri-userinfo uri))
            (port (uri-port uri))
            (path (uri-path uri))
            (query (uri-query uri))
            (fragment (uri-fragment uri)))
        (let ((uri-string
               (concatenate 'string
                            (when scheme
                              scheme)
                            (when scheme ":")
                            (when host
                              "//")
                            (when userinfo
                              (percent-encode-string
                               userinfo
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)
                                              (eq c #\:)))))
                            (when userinfo
                              "@")
                            (when host
                              (percent-encode-string
                               host
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)))))
                            (when port
                              ":")
                            (when port
                              (format nil "~A" port))
                            (when path
                              (percent-encode-string
                               path
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)
                                              (eq c #\:)
                                              (eq c #\@)
                                              (eq c #\/)))))
                            (when query
                              "?")
                            (when query
                              (percent-encode-string
                               query
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)
                                              (eq c #\:)
                                              (eq c #\@)
                                              (eq c #\/)
                                              (eq c #\?)))))
                            (when fragment
                              "#")
                            (when fragment
                              (percent-encode-string
                               fragment
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)
                                              (eq c #\:)
                                              (eq c #\@)
                                              (eq c #\/)
                                              (eq c #\?))))))))
          (setf (uri-string uri) uri-string))))
    (uri-string uri)))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity t)
    (format stream "~S" (render-uri uri))))
