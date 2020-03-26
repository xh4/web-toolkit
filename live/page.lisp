(in-package :live)

(defclass page-class (reactive-class) ())

(define-reactive-class page ()
  ((title
    :initform nil)
   (favicon
    :initform nil)
   (content
    :initform nil)
   (%content
    :initform nil)
   (version
    :initform 0))
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
    (without-propagation
      (setf (slot-value page '%content) (render-all component)
            (slot-value page 'content) component))))

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
                 styles)
                (html:body
                 content
                 (html:script
                  (ps*
                   `(progn
                      (defun handle-raw-message (message)
                        (try
                         (setf message (chain -j-s-o-n (parse message)))
                         (:catch (e)
                                 (chain console (error "Unable to parse message" message))
                                 (chain console (error e))
                                 (return)))
                        (unless (chain -Array (is-array message))
                          (chain console (error "Unable to handle message" message)))
                        (when (= (@ message length) 0)
                          (chain console (error "Unable to handle message" message)))
                        (handle-message message))

                      (defun handle-message (message)
                        (chain console (log "Receive" message))
                        (let ((type (@ message 0)))
                          (case type
                            ("replace" (replace (@ message 1) (@ message 2)))
                            ("update" (update (@ message 1) (@ message 2)))
                            ("remove" (remove (@ message 1)))
                            ("insert" (insert (@ message 1) (@ message 2) (@ message 3))))))

                      (defun replace (selector node-string)
                        (let ((new-node (chain (new (-d-o-m-parser))
                                           (parse-from-string node-string "text/html")
                                           body
                                           first-child)))
                          (let ((old-node (@ document body)))
                            (for-in (i selector)
                                    (setf old-node (getprop old-node 'child-nodes (getprop selector i))))
                            (chain console (log "Replace" selector old-node new-node))
                            (chain old-node (replace-with new-node)))))

                      (defun update (selector attributes)
                        (let ((node (@ document body)))
                          (for-in (i selector)
                                  (setf node (getprop node 'child-nodes (getprop selector i))))
                          (chain console (log "Update" selector node attributes))
                          (for-in (name attributes)
                                  (let ((value (getprop attributes name)))
                                    (if value
                                        (chain node (set-attribute name value))
                                        (chain node (remove-attribute name)))))))

                      (defun remove (selector)
                        (let ((node (@ document body)))
                          (for-in (i selector)
                                  (setf node (getprop node 'child-nodes (getprop selector i))))
                          (chain console (log "Remove" selector node))
                          (chain node parent-node (remove-child node))))

                      (defun insert (selector index node-string)
                        (let ((new-node (chain (new (-d-o-m-parser))
                                               (parse-from-string node-string "text/html")
                                               body
                                               first-child)))
                          (let ((parent-node (@ document body)))
                            (for-in (i selector)
                                    (setf parent-node
                                          (getprop parent-node 'child-nodes (getprop selector i))))
                            (chain console (log "Insert" selector parent-node index new-node))
                            (chain parent-node (insert-before new-node
                                                              (getprop parent-node 'children index))))))

                      (defvar url (+ "ws://"
                                     (@ location "host")
                                     (@ location "pathname")))
                      (defvar ws (new (-web-socket url)))
                      (chain ws (add-event-listener
                                 "open"
                                 (lambda (m)
                                   (chain console (log "Open" url)))))
                      (chain ws (add-event-listener
                                 "message"
                                 (lambda (event)
                                   (chain console (log "Message" (@ event data)))
                                   (handle-raw-message (@ event data)))))
                      (chain ws (add-event-listener
                                 "close"
                                 (lambda (event)
                                   (chain console (log "Close" event)))))
                      (chain ws (add-event-listener
                                 "error"
                                 (lambda (event)
                                   (chain console (log "Error" event)))))))))))))))))))

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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,page-name ,superclasses
       ,slots
       ,@options)))

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
  (incf (slot-value page 'version)))

(defvar *actions* nil)

(defmethod react ((session page-session) (page page))
  ;; (format t "Update page session ~A for page ~A~%" session page)
  (when (session-open-p session)
    (let* ((old-content (html:body (slot-value page '%content))))
      (multiple-value-bind (new-content render-table)
          (render-all (html:body (page-content page)))
        (let ((actions (com::diff old-content new-content)))
          (setf *actions* actions)
          (format t "~A~%" actions)
          (loop for action in actions
             for type = (first action)
             for selector = (second action)
             for message = (case type
                             (:replace (json:array "replace"
                                                   selector
                                                   (html:serialize (fourth action))))
                             (:update (json:array "update"
                                                  selector
                                                   (let ((object (json:object)))
                                                     (loop for (name . value) in (fourth action)
                                                        do (setf (json:get object name) value))
                                                     object)))
                             (:remove (json:array "remove" selector))
                             (:insert (json:array "insert" selector
                                                  (fourth action)
                                                  (html:serialize (fifth action)))))
             do (send-text session (json:encode message))))))))
