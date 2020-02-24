(in-package :uri)

(defgeneric uri-string (uri)
  (:method ((uri uri))
    (if-let ((uri-string (slot-value uri 'string)))
      uri-string
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
                              (percent-encode
                               userinfo
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)
                                              (eq c #\:)))))
                            (when userinfo
                              "@")
                            (when host
                              (percent-encode
                               host
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)))))
                            (when port
                              ":")
                            (when port
                              (format nil "~A" port))
                            (when path
                              (percent-encode
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
                              query)
                            (when fragment
                              "#")
                            (when fragment
                              (percent-encode
                               fragment
                               :reserve (lambda (c)
                                          (or (unreserved-p c)
                                              (sub-delim-p c)
                                              (eq c #\:)
                                              (eq c #\@)
                                              (eq c #\/)
                                              (eq c #\?))))))))
          (setf (slot-value uri 'string) uri-string))))))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity t)
    (format stream "~S" (uri-string uri))))
