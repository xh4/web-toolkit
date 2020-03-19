(in-package :live)

(defclass page-class (reactive-class) ())

(defclass page (reactive-object)
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
  (:method-class reactive-method))

(defgeneric (setf page-title) (value page)
  (:method-class reactive-method))

(defgeneric page-favicon (page)
  (:method-class reactive-method))

(defgeneric (setf page-favicon) (value page)
  (:method-class reactive-method))

(defgeneric page-content (page)
  (:method-class reactive-method))

(defgeneric (setf page-content) (value page)
  (:method-class reactive-method))

(defmethod page-content :around ((page page))
  (let ((component (call-next-method)))
    (when (reactive-object-p component)
      (add-dependency page component))
    (when (reactive-object-p (class-of component))
      (add-dependency page (class-of component)))
    component))

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
      (page-content page))))
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
         (let ((rules (compute-style-rules content)))
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
               (equal "GET"
                      (request-method request)))
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

;; (defmethod reflect ((component component) (class component-class) update)
;;   (update component t))

;; (defmethod reflect ((component component) (variable variable) update)
;;   (format t "Update component ~A for variable ~A~%" component variable)
;;   (update component t))

;; (defmethod reflect ((page page) (component component) update)
;;   (format t "Update page ~A for component ~A~%" page component)
;;   (update page t))

;; (defmethod reflect ((page page) (class component-class) update)
;;   (format t "Update page ~A for component class ~A~%" page class)
;;   (update page t))

;; (defmethod reflect ((page page) (class page-class) update)
;;   (format t "Update page ~A for page class ~A~%" page class)
;;   (update page t))

;; (defmethod reflect ((session page-session) (page page) update)
;;   (format t "Update session ~A for page ~A~%" session page)
;;   (ws:send-text session "reload"))
