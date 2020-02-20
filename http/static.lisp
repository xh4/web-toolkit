(in-package :http)

(define-handler static-handler ()
  ((prefix
    :initarg :prefix
    :initform nil)
   (root
    :initarg :root
    :initform nil))
  (:function
   (lambda (handler request)
     (with-slots (prefix root) handler
       (let ((pathname (resolve-path prefix root (uri-path (request-uri request)))))
         (if (directory-pathname-p pathname)
             (reply pathname)
             (let ((pathname (probe-file pathname)))
               (if (directory-pathname-p pathname)
                   (let ((uri (uri (request-uri request))))
                     (setf (uri-path uri) (format nil "~A/" (uri-path uri)))
                     (redirect (uri-string uri)))
                   (reply pathname)))))))))

(defclass static-route (route)
  ((prefix
    :initarg :prefix
    :initform nil)
   (root
    :initarg :root
    :initform nil)))

(defmethod route ((type (eql :static)) form)
  (apply 'make-static-route (rest form)))

;; TODO: Check path normativity
(defun path-segments (path)
  (split-sequence #\/ (subseq path 1)))

(defun path-prefix-p (prefix path)
  (if (or (null prefix)
          (equal prefix "")
          (equal prefix "/"))
      t
      (let ((path-segments (path-segments (uri::remove-dot-segments path)))
            (prefix-segments (split-sequence #\/ (string-trim "/" prefix))))
        (loop for pos upto (1- (min (length prefix-segments)
                                    (length path-segments)))
           for prefix-segment = (nth pos prefix-segments)
           for path-segment = (nth pos path-segments)
           unless (equal prefix-segment path-segment)
           do (return nil)
           finally (return t)))))

(defun path-trim-prefix (prefix path)
  (if (or (null prefix)
          (equal prefix "")
          (equal prefix "/"))
      (subseq (uri::remove-dot-segments path) 1)
      (let ((path-segments (path-segments (uri::remove-dot-segments path)))
            (prefix-segments (split-sequence #\/ (string-trim "/" prefix)))
            (suffix-segments '())
            (match-prefix-p nil))
        (loop for end from 1 upto (length path-segments)
           for head-segments = (subseq path-segments 0 end)
           for tail-segments = (subseq path-segments end)
           when (equal head-segments prefix-segments)
           do (setf suffix-segments tail-segments
                    match-prefix-p t))
        (when match-prefix-p
          (if (null suffix-segments)
              ""
              (format nil "~{~A~^/~}" suffix-segments))))))

(defun resolve-path (prefix root path)
  (let ((relative-path (path-trim-prefix prefix path)))
    (setf relative-path (uri::percent-decode relative-path))
    (let ((pathname (merge-pathnames relative-path root)))
      pathname)))

(defun make-static-route (&key prefix root)
  (setf prefix (eval prefix))
  (setf root (eval root))
  (check-type root (or string pathname))
  (when (stringp root)
    (setf root (pathname root)))
  (unless (uiop:absolute-pathname-p root)
    (error "Root must be absolute"))
  (setf root (uiop:ensure-directory-pathname root))

  (let ((matcher (lambda (request)
                   (and (or (eq (request-method request) :get)
                            (equal (request-method request) "GET"))
                        (path-prefix-p prefix (uri-path (request-uri request))))))
        (handler (make-instance 'static-handler
                                :prefix prefix
                                :root root)))
    (make-instance 'static-route
                   :prefix prefix
                   :root root
                   :matcher matcher
                   :handler handler)))
