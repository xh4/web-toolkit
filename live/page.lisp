(in-package :live)

(defclass page-class (reactive-class) ())

(define-reactive-class page ()
  ((title
    :initarg :title
    :initform nil)
   (favicon
    :initarg :favicon
    :initform nil)
   (content
    :initarg :content
    :initform nil))
  (:metaclass page-class))

(defgeneric page-title (page)
  (:method ((page page))
    (symbol-name (class-name (class-of page)))))

(defgeneric (setf page-title) (value page))

(defgeneric page-favicon (page))

(defgeneric (setf page-favicon) (value page))

(defgeneric page-content (page)
  (:method ((page page))))

(defgeneric (setf page-content) (value page))

(defmethod page-content :around ((page page))
  (let ((component (call-next-method)))
    (when (typep component 'reactive-object)
      (add-dependency page component))
    (with-propagation
      (setf (slot-value page 'content) component))))

(defgeneric initialize-page (page request)
  (:method ((page page) request)))

(defmethod serialize ((page page) &optional stream)
  (serialize
   (html:document
    (html:html
     (html:head
      (html:meta :charset "utf-8")
      (html:title (page-title page)))
     (html:body
      (slot-value page 'content))))
   stream))

(define-handler page-handler ()
  ((page
    :initarg :page
    :initform nil
    :accessor handler-page))
  (:instanize nil)
  (:function
   (lambda (handler request)
     (let ((page (handler-page handler)))
       (let ((title (page-title page))
             (content (page-content page)))
         (let ((rules (com::compute-style-rules content)))
           (let ((styles (loop for rule in rules
                            collect (html:style (css::serialize rule)))))
             (reply
              (html:document
               (html:html
                (html:head
                 (html:meta :charset "utf-8")
                 (html:title title)
                 styles
                 (html:script :src "/page.js"))
                (html:body content)))))))))))

(define-session page-session (reactive-object)
  ((page
    :initarg :page
    :initform nil
    :accessor session-page)))

(define-endpoint page-endpoint ()
  ((page
    :initarg :page
    :initform nil
    :accessor endpoint-page))
  (:session-class 'page-session)
  (:on-open (lambda (endpoint session)
              (let ((page (endpoint-page endpoint)))
                (page-content page)
                (setf (session-page session) page)
                (add-dependency session page))))
  (:instanize nil))

(defmacro define-page (page-name superclasses slots &rest options)
  (unless (find 'page superclasses)
    (appendf superclasses '(page)))
  (unless (find :metaclass options :key 'first)
    (rewrite-class-option options :metaclass page-class))
  #+lispworks
  (rewrite-class-option options :optimize-slot-access nil)
  `(defclass ,page-name ,superclasses
     ,slots
     ,@options))

(defclass page-route (route)
  ((page-class
    :initarg :page-class
    :initform nil
    :accessor route-page-class)))

(defmethod route ((route page-route) request)
  (let ((page-class (find-class (route-page-class route))))
    (when (and (equal (format nil "/~(~A~)" (class-name page-class))
                      (uri-path request))
               (equal "GET" (request-method request)))
      (let ((page (make-instance page-class)))
        (initialize-page page request)
        ;; TODO: case insensitive
        (if (and (search "Upgrade" (header-field-value
                                    (find-header-field "Connection" request)))
                 (string-equal "WebSocket" (header-field-value
                                            (find-header-field "Upgrade" request))))
            (make-instance 'page-endpoint :page page)
            (make-instance 'page-handler :page page))))))

(defmethod make-route ((type (eql :page)) form)
  (let ((page-class (second form)))
    (unless (find-class page-class)
      (error "Class ~A not found" page-class))
    (unless (subtypep page-class 'page)
      (error "Class ~A is not a subclass of PAGE" page-class))
    (make-instance 'page-route
                   :page-class page-class)))

(defmethod react ((page page) (component component))
  ;; (format t "Update page ~A for component ~A~%" page component)
  (page-content page))

(defmethod react ((session page-session) (page page))
  ;; (format t "Update page session ~A for page ~A~%" session page)
  (send-text session "update"))
