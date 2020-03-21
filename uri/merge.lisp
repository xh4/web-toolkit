(in-package :uri)

(defgeneric uri-absolute-p (uri)
  (:method ((uri uri))
    (and (uri-scheme uri)))
  (:method ((uri string))
    (uri-absolute-p (uri uri))))

(defun uri-relative-p (uri)
  (not (uri-absolute-p uri)))

(defgeneric uri-path-segments (uri)
  (:method ((uri uri))
    (split-sequence #\/ (uri-path uri)))
  (:method ((uri string))
    (uri-path-segments (uri uri))))

(defun merge-path (base-uri relative-uri)
  (if (and (uri-host base-uri)
           (= (length (uri-path base-uri)) 0))
      (concatenate 'string "/" (uri-path relative-uri))
      (format nil "~{~A~^/~}"
              (append (butlast (uri-path-segments base-uri))
                      (uri-path-segments relative-uri)))))

(defgeneric remove-dot-segments (segments)
  (:method ((segments list))
    (let* ((last-segment (last segments))
           (dot-end-p (equal last-segment '("."))))
      (setf segments (remove "." segments :test #'equal))
      (when dot-end-p (appendf segments '(""))))
    (loop for up-position = (position ".." segments :test #'equal)
       while up-position
       finally (return (format nil "~{~A~^/~}" segments))
       do (let* ((x (nthcdr (1- up-position) segments))
                 (y (or (cddr x) '(""))))
            (when (eq x segments) (setq x (rest segments)))
            (setq segments (append (ldiff segments x) y)))))
  (:method ((segments string))
    (remove-dot-segments (split-sequence #\/ segments))))

(defgeneric merge-uri (base-uri relative-uri)
  (:method ((base-uri uri) (relative-uri uri))
    (if (or (null base-uri)
            (not (uri-relative-p relative-uri)))
        relative-uri
        (progn
          (unless (uri-scheme base-uri)
            (error 'merge-uri-error
                   :base-uri base-uri
                   :relative-uri relative-uri
                   :message (format nil "Missing scheme component in base URI ~A" base-uri)))
          (let ((target-uri (make-instance 'uri)))
            (with-slots ((b-scheme scheme) (b-userinfo userinfo)
                         (b-host host) (b-port port) (b-path path)
                         (b-query query)) base-uri
              (with-slots ((r-scheme scheme) (r-userinfo userinfo)
                           (r-host host) (r-port port) (r-path path)
                           (r-query query) (r-fragment fragment)) relative-uri
                (with-slots ((t-scheme scheme) (t-userinfo userinfo)
                             (t-host host) (t-port port) (t-path path)
                             (t-query query) (t-fragment fragment)) target-uri
                  (if r-scheme
                      (setf t-scheme r-scheme
                            t-userinfo r-userinfo
                            t-host r-host
                            t-port r-port
                            t-path (remove-dot-segments r-path)
                            t-query r-query)
                      (progn
                        (if r-host
                            (setf t-userinfo r-userinfo
                                  t-host r-host
                                  t-port r-port
                                  t-path (remove-dot-segments r-path)
                                  t-query r-query)
                            (progn
                              (if (= (length r-path) 0)
                                  (progn
                                    (setf t-path b-path)
                                    (if r-query
                                        (setf t-query r-query)
                                        (setf t-query b-query)))
                                  (progn
                                    (if (let ((pos (position #\/ r-path)))
                                          (and pos (= pos 0)))
                                        (setf t-path (remove-dot-segments r-path))
                                        (setf t-path (merge-path base-uri relative-uri)
                                              t-path (remove-dot-segments t-path)))
                                    (setf t-query r-query)))
                              (setf t-userinfo b-userinfo
                                    t-host b-host
                                    t-port b-port)))
                        (setf t-scheme b-scheme)))
                  (setf t-fragment r-fragment))))
            target-uri))))
  (:method (base-uri relative-uri)
    (merge-uri (uri base-uri) (uri relative-uri))))
