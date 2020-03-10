(in-package :uri)

(defun render-uri (uri)
  (check-type uri (or uri null))
  (unless uri (return-from render-uri nil))
  (if-let ((uri-string (slot-value uri 'string)))
    uri-string
    (let ((scheme (slot-value uri 'scheme))
          (host (slot-value uri 'host))
          (userinfo (slot-value uri 'userinfo))
          (port (slot-value uri 'port))
          (path (slot-value uri 'path))
          (query (slot-value uri 'query))
          (fragment (slot-value uri 'fragment)))
      (let ((uri-string
             (concatenate 'string
                          (when scheme
                            scheme)
                          (when scheme ":")
                          (when host
                            "//")
                          userinfo
                          (when userinfo
                            "@")
                          host
                          (when port
                            ":")
                          (when port
                            (format nil "~A" port))
                          path
                          (when query
                            "?")
                          query
                          (when fragment
                            "#")
                          fragment)))
        (when (and (plusp (length uri-string))
                   (eq #\? (char uri-string 0)))
          (setf uri-string (subseq uri-string 1)))
        (setf (slot-value uri 'string) uri-string)))))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity t)
    (format stream "~S" (render-uri uri))))
