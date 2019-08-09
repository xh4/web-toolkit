(in-package :http)

(define-handler router ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor router-rules)))

(defmethod print-object ((router router) stream)
  (print-unreadable-object (router stream :type t :identity t)
    (loop for rule in (router-rules router)
         do (format stream "~%  ~A" rule))))

(defmacro define-router (name)
  `(if (boundp ',name)
       (progn
         (setf ,name (make-instance 'router))
         ,name)
       (defvar ,name
         (make-instance 'router))))

(defclass routing-rule ()
  ((matcher
    :initarg :matcher
    :initform nil
    :accessor routing-rule-matcher)
   (handler
    :initarg :handler
    :initform nil
    :accessor routing-rule-handler)))

(defgeneric build-routing-rule (type form))

(defclass verbose-routing-rule (routing-rule)
  ((method
    :initarg :method
    :initform nil
    :accessor routing-rule-method)
   (path
    :initarg :path
    :initform nil
    :accessor routing-rule-path)))

(defmethod build-routing-rule ((type (eql :get)) form)
  (apply #'build-verbose-routing-rule form))
(defmethod build-routing-rule ((type (eql :post)) form)
  (apply #'build-verbose-routing-rule form))
(defmethod build-routing-rule ((type (eql :put)) form)
  (apply #'build-verbose-routing-rule form))
(defmethod build-routing-rule ((type (eql :delete)) form)
  (apply #'build-verbose-routing-rule form))

(defun build-verbose-routing-rule (method path handler)
  (let ((rule (make-instance 'verbose-routing-rule
                             :method method
                             :path path
                             :handler (gethash handler *handler-mapping-table*))))
    (let ((matcher (lambda (request)
                     (unless (equal (routing-rule-path rule) (request-uri request))
                       (format t "Path: ~A | ~A~%"
                               (routing-rule-path rule)
                               (request-uri request)))
                     (unless (eq (routing-rule-method rule) (request-method request))
                       (format t "Method: ~A | ~A~%"
                               (routing-rule-method rule)
                               (request-method request)))
                     (and (equal (routing-rule-path rule) (request-uri request))
                          (eq (routing-rule-method rule) (request-method request))))))
      (setf (routing-rule-matcher rule) matcher))
    rule))

(defmethod build-routing-rule ((type (eql :static)) form)
  (apply #'build-static-routing-rule (rest form)))

;; TODO: Check path normativity
(defun path-segments (path)
  (split-sequence #\/ (subseq path 1)))

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

;; (path-trim-prefix "foo" "/foo")
;; (path-trim-prefix "foo" "/foo/")
;; (path-trim-prefix "foo" "/foo/bar")
;; (path-trim-prefix "/foo" "/foo/bar")
;; (path-trim-prefix "foo/" "/foo/bar")
;; (path-trim-prefix "/foo/" "/foo/bar")
;; (path-trim-prefix "/foo/bar" "/foo/bar")
;; (path-trim-prefix "/foo/bar" "/foo/bar/")
;; (path-trim-prefix "/foo/bar/" "/foo/bar")
;; (path-trim-prefix "/foo/bar/" "/foo/bar/")
;; (path-trim-prefix "goo" "/foo/bar/")
;; (path-trim-prefix "/" "/foo/bar/")
;; (path-trim-prefix "" "/foo/bar")
;; (path-trim-prefix nil "/foo/bar/")

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

(defmacro router (&rest rule-forms)
  (let ((rules (loop for rule-form in rule-forms
                  collect
                    (progn
                      (unless (listp rule-form)
                        (error "Illformed rule form: ~A, expect a list" rule-form))
                      (let ((type (car rule-form)))
                        (unless (symbolp type)
                          (error "Illformed rule form: ~A, expect a symbol at the head" rule-form))
                        (setf type (make-keyword type))
                        (unless (find-method #'build-routing-rule
                                             '()
                                             (list `(eql ,type)
                                                   (find-class t))
                                             nil)
                          (error "No method to build routing rule for form ~A" rule-form))
                        (build-routing-rule type rule-form))))))
    (make-instance 'router :rules rules)))

(defun handle-missing (request)
  (declare (ignore request))
  (setf (response-status *response*) 404)
  (setf (header-field *response* "Content-Type") "text/plain")
  (setf (response-body *response*) "not found"))

(defmethod handle ((router router) (request request))
  (let ((target-rule nil))
    (loop for rule in (router-rules router)
       for matcher = (routing-rule-matcher rule)
       when (funcall matcher request)
       do
         (setf target-rule rule)
         (return))
    (if target-rule
        (let ((handler (routing-rule-handler target-rule)))
          (invoke-handler handler request))
        (handle-missing request))))
