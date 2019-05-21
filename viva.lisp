(in-package :cl-user)

(ql:quickload :hunchentoot)
(ql:quickload :wt.websocket)
(ql:quickload :wt.json)
(ql:quickload :wt.component)
(ql:quickload :wt.bootstrap)
(ql:quickload :cl-who)
(ql:quickload :parenscript)

(setf (cl-who:html-mode) :html5)

(defvar *acceptor* nil)

(defun start ()
  (when (null *acceptor*)
    (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4001))
    (handler-case
        (hunchentoot:start *acceptor*)
      (error (e)
        (setf *acceptor* nil)
        (format t "~A~%" e)))))

(defun stop ()
  (when *acceptor*
    (handler-case
        (hunchentoot:stop *acceptor*)
      (error (e)
        (format t "~A~%" e)))
    (setf *acceptor* nil)))

(hunchentoot:define-easy-handler (index :uri "/") ()
    (setf (hunchentoot:content-type*) "text/html")
    (cl-who:with-html-output-to-string (html nil :prologue t :indent t)
      (:html
       (:head
        (:meta :charset "utf-8"))
       (:body
        (:script :src "/static/jquery-3.4.0.js")
        (:script :src "/static/wt.js")
        (:script :src "/static/index.js")))))

(push
 (hunchentoot:create-folder-dispatcher-and-handler
  "/static/"
  (merge-pathnames
   "viva/"
   (asdf/system:system-source-directory
    (asdf/system:find-system "wt.websocket"))))
 hunchentoot:*dispatch-table*)

(ws:define-session session ())

(defun read-object-value (object)
  (let ((type (json:get object "type")))
    (alexandria:switch (type :test 'equal)
      ("symbol"
       (let ((name (json:get object "name"))
             (package (json:get object "package")))
         (find-symbol name))))))

(defun read-value (value)
  (typecase value
    ((or string number) value)
    (json:object (read-object-value value))
    (t nil)))

(defun handle-call (symbol))

(defmethod ws:on-message ((session session) message)
  (format t "Receive: ~A~%" message)
  (handler-case
      (let ((message (json:decode-json message)))
        (let ((name (first message)))
          (alexandria:switch (name :test 'equal)
            ("call"
             (let ((symbol (read-value (second message)))
                   (arguments (mapcar 'read-value (cddr message))))
               (let ((result (apply symbol arguments)))
                 (format t "~A~%" result)))))))
    (error (e)
      (format t "~A~%" e))))

(ws:define-endpoint endpoint
    :path "/"
    :session-class 'session)

(style:define-css-source-file bootstrap-4.3.1.css
    "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")

(defmacro ps-load-css (href)
  `(let ((link (ps:chain document (create-element "link"))))
     (ps:chain link (set-attribute "rel" "stylesheet"))
     (ps:chain link (set-attribute "type" "text/css"))
     (ps:chain link (set-attribute "href" ,href))
     (ps:chain document
               (get-elements-by-tag-name "head")
               0
               (append-child link))))

(defmethod ws:on-open ((endpoint endpoint) session)
  (let* ((button (bs:button :style :danger
                            :outline-p t
                            :size :large
                            :block-p t
                            (com:text "foo")))
         (code (ps:ps
                (progn
                   (ps-load-css "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
                   (ps:chain ($ "body") (html ,(html:serialize
                                                (eval (com:expand-all button))))))))
         (message (list "eval" code)))
    (ws:send-text session (json:encode-json message))))

(defmethod ws:on-close ((endpoint endpoint) &optional reason)
  (format t "Close: ~A~%" reason))

(defmethod ws:on-error ((endpoint endpoint) error)
  (format t "Error: ~A~%" error))

(ws:define-server websocket-server
    :port 4002
    :endpoints (list endpoint))

;; (start)
;; (ws:start-server websocket-server)

;; (stop)
;; (ws:stop-server websocket-server)
