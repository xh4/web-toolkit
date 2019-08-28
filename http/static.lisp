(in-package :http)

(define-handler static-handler ()
  ((prefix
    :initarg :prefix
    :initform nil)
   (location
    :initarg :location
    :initform nil)))

(defmethod handle ((handler static-handler) (request request))
  (let* ((prefix (slot-value handler 'prefix))
         (location (slot-value handler 'location))
         (relative-path (path-trim-prefix prefix (request-uri request)))
         (pathname (merge-pathnames relative-path location)))
    (setf (response-body *response*) pathname)))

(defmethod build-routing-rule ((type (eql :static)) form)
  (apply #'build-static-routing-rule (rest form)))

;; TODO: Check path normativity
(defun path-segments (path)
  (split-sequence #\/ (subseq path 1)))

(defun path-prefix-p (prefix path)
  (if (or (null prefix)
          (equal prefix "")
          (equal prefix "/"))
      t
      (let ((path-segments (path-segments path))
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
      (subseq path 1)
      (let ((path-segments (path-segments path))
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

(defun build-static-routing-rule (&key prefix location)
  (check-type location (or string pathname))
  (when (stringp location)
    (setf location (pathname location)))
  (unless (uiop:absolute-pathname-p location)
    (error "Location must be absolute"))
  (setf location (uiop:ensure-directory-pathname location))

  (let ((rule (make-instance 'routing-rule)))
    (let ((matcher (lambda (request)
                     (and (eq (request-method request) :get)
                          (path-trim-prefix prefix (request-uri request))))))
      (setf (routing-rule-matcher rule) matcher))
    (let ((handler (make-instance 'static-handler
                                  :prefix prefix
                                  :location location)))
      (setf (routing-rule-handler rule) handler))
    rule))
