(in-package :cl-user)

(ql:quickload :hunchentoot)
(ql:quickload :wt.websocket)
(ql:quickload :cl-who)

(setf (cl-who:html-mode) :html5)

(defvar *acceptor* nil)

(defun start ()
  (when (null *acceptor*)
    (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4000))
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

(defmethod ws:on-message ((session session) message)
  (format t "Received: ~A~%" message)
  (ws:send-text session message))

(ws:define-endpoint endpoint
    :path "/"
    :session-class 'session)

(defmethod ws:on-open ((endpoint endpoint) session)
  (ws:send-text session "[\"eval\", \"console.log('haha')\"]"))

(defmethod ws:on-close ((endpoint endpoint) &optional reason)
  (format t "Close: ~A~%" reason))

(defmethod ws:on-error ((endpoint endpoint) error)
  (format t "Error: ~A~%" error))

(ws:define-server websocket-server
    :port 4001
    :endpoints (list endpoint))

;; (start)
;; (ws:start-server websocket-server)

;; (stop)
;; (ws:stop-server websocket-server)
