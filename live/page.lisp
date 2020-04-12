(in-package :live)

(defclass page-class (reactive-class) ())

(defvar *page-initializing* nil)

(defmethod (setf slot-value-using-class) :around (value (class page-class) page slot)
  (declare (ignore value))
  (typecase slot
    (symbol slot)
    (slot-definition (setf slot (slot-definition-name slot))))
  (if *page-initializing*
      (without-propagation (call-next-method))
      (if (eq 'version slot)
          (call-next-method)
          (prog1
              (without-propagation (call-next-method))
            (when (find slot (class-direct-slots class)
                        :key 'slot-definition-name)
              (incf (slot-value page 'version)))))))

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
  (:metaclass page-class)
  #+lispworks
  (:optimize-slot-access nil))

(defmethod shared-initialize :around ((page page) slot-names
                                      &key &allow-other-keys)
  (declare (ignore slot-names))
  (let ((*page-initializing* t))
    (call-next-method)))

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
  (let ((current-content (slot-value page 'content)))
    (when (typep current-content 'reactive-object)
      (remove-dependency page current-content))
    (let ((new-content (call-next-method)))
      (when (typep new-content 'reactive-object)
        (add-dependency page new-content))
      (without-propagation
        (setf (slot-value page '%content) (render-all new-content)
              (slot-value page 'content) new-content)))))

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
         (multiple-value-bind (root render-tree component-list)
             (render-all content)
           (reply
            (html:document
             (html:html
              (html:head
               (html:meta :charset "utf-8")
               (html:title title)
               (compute-component-style-elements component-list)
               (html:script :src "https://code.jquery.com/jquery-3.4.1.js"))
              (html:body
               root
               (html:script
                (ps*
                 `(progn
                    (defvar *style* (create))

                    (defun make-styles ()
                      (for-in (component-name *style*)
                              (let ((text "")
                                    (selectors (getprop *style* component-name)))
                                (for-in (selector selectors)
                                        (let ((style0 (getprop selectors selector)))
                                          (let ((property (+ selector " {")))
                                            (for-in (property-name style0)
                                                    (let ((property-value (getprop style0 property-name)))
                                                      (setf property (+ property #\Newline "  "
                                                                        (+ property-name ": " property-value ";")))))
                                            (setf property (+ property #\Newline "}"))
                                            (setf text (+ text #\Newline property #\Newline)))))
                                (let ((node (chain document (query-selector (+ "style[component=" component-name "]")))))
                                  (if node
                                      (setf (@ node text-content) text)
                                      (let ((node (chain document (create-element "style"))))
                                        ;; (setf (@ node type) "text/css")
                                        (chain node (set-attribute "component" component-name))
                                        (setf (@ node text-content) text)
                                        (chain document head (append-child node))))))))

                    (defun handle-raw-message (message)
                      (try
                       (setf message (chain -j-s-o-n (parse message)))
                       (:catch (e)
                               (chain console (error "Unable to parse message" message))
                               (chain console (error e))
                               (return-from handle-raw-message)))
                      (unless (chain -Array (is-array message))
                        (chain console (error "Malformed message" message))
                        (return-from handle-raw-message))
                      (when (= (@ message length) 0)
                        (chain console (error "Unable to handle empty message" message))
                        (return-from handle-raw-message))
                      (handle-message message))

                    (defun handle-message (message)
                      (chain console (log "Receive" message))
                      (let ((type (@ message 0)))
                        (case type
                          ("reconciliation" (handle-reconciliation (@ message 1)))
                          ("style" (handle-style (@ message 1)))
                          (t (chain console (error "Unable to handle message with type" type message))))))

                    (defun handle-reconciliation (actions)
                      (loop for action in actions
                         for type = (@ action 0)
                         do (case type
                              ("replace" (replace (@ action 1) (@ action 2) (@ action 3)))
                              ("update" (update (@ action 1) (@ action 2)))
                              ("remove" (remove (@ action 1)))
                              ("insert" (insert (@ action 1) (@ action 2) (@ action 3) (@ action 4))))))

                    (defun replace (selector node-type node-string)
                      (let ((new-node (if (= node-type "text")
                                          (chain document (create-text-node node-string))
                                          (chain $ (parse-h-t-m-l node-string) (pop)))))
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

                    (defun insert (selector index node-type node-string)
                      (let ((new-node (if (= node-type "text")
                                          (chain document (create-text-node node-string))
                                          (chain $ (parse-h-t-m-l node-string) (pop)))))
                        (let ((parent-node (@ document body)))
                          (for-in (i selector)
                                  (setf parent-node
                                        (getprop parent-node 'child-nodes (getprop selector i))))
                          (chain console (log "Insert" selector parent-node index new-node))
                          (chain parent-node (insert-before new-node
                                                            (getprop parent-node 'children index))))))

                    (defun handle-style (style)
                      (setf *style* style)
                      (make-styles))

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
                                 (chain console (log "Error" event))))))))))))))))))

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

(defun component-class-style-element (component-name)
  (let ((text (with-output-to-string (stream)
                (loop for rule in (component-class-style component-name)
                   do
                     (format stream "~%")
                     (css:serialize rule stream)
                     (format stream "~%")))))
    (html:style
     :component (format nil "~(~A~)" component-name)
     :package (format nil "~(~A~)" (package-name (symbol-package component-name)))
     text)))

(defun compute-component-style-object (component-list)
  (let ((class-list (mapcar #'class-of component-list)))
    (setf class-list (sort class-list (lambda (a b)
                                        (subtypep b a))))
    (let ((groups (group-by class-list :key (lambda (class)
                                              (package-name
                                               (symbol-package
                                                (class-name class)))))))
      (let ((package-object (json:object)))
        (loop for group in groups
             for package-name = (format nil "~(~A~)" (first group))
             do (setf (get package-object package-name) t))))))

(defun compute-component-style-elements (component-list)
  (let ((class-list (mapcar #'class-of component-list)))
    (setf class-list (sort class-list (lambda (a b)
                                        (subtypep b a))))
    (loop for class in class-list
       collect (component-class-style-element (class-name class)))))

(defun style-rule-object (rule)
  (let ((selector (format nil "~{~A~^, ~}" (ensure-list (rule-selector rule)))))
    (json:object selector (apply json'object
                                 (loop for declaration in (rule-declarations rule)
                                       collect (list (declaration-name declaration)
                                                     (declaration-value declaration)))))))

(defun merge-object (object-1 object-2)
  )

(defun component-style-object (component)
  (let ((component-name (string-downcase (class-name (class-of component))))
        (rules (component-class-style (class-name (class-of component))))
        (style (json:object)))
    (loop with properties = (json:object)
          for rule in rules
          for selector = (rule-selector rule)
          do (loop for property in (rule-declarations rule)
                   for name = (format nil "~(~A~)" (property-name property))
                   for value = (property-value property)
                   do (setf (json:get properties name) value))
          finally (setf (json:get style selector)
                        properties)
          (return style))))

(defmethod react ((session page-session) (page page))
  ;; (format t "Update page session ~A for page ~A~%" session page)
  (when (session-open-p session)
    ;; (format t "Page version: ~A~%" (slot-value page 'version))
    (let ((old-content (html:body (slot-value page '%content))))
      (multiple-value-bind (new-content render-table)
          (render-all (html:body (page-content page)))
        (let ((actions (diff old-content new-content)))
          (setf *actions* actions)
          ;; (format t "~A~%" actions)

          ;; (loop with style = (json:object)
          ;;    for component in (hash-table-keys render-table)
          ;;    for component-name = (string-downcase (class-name (class-of component)))
          ;;    for rules = (component-style-object component)
          ;;    do (setf (json:get style component-name) rules)
          ;;    finally (send-text session (json:encode (json:array "style" style))))

          (loop for action in actions
             for type = (first action)
             for selector = (second action)
             for js-action = (case type
                               (:replace (json:array "replace"
                                                     selector
                                                     (typecase (fourth action)
                                                       (html:element "element")
                                                       ((or html:text string) "text"))
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
                                                    (typecase (fifth action)
                                                      (html:element "element")
                                                      ((or html:text string) "text"))
                                                    (html:serialize (fifth action)))))
             collect js-action into js-actions
             finally (let ((js-message (json:array "reconciliation" js-actions)))
                       (send-text session (json:encode js-message)))))))))
