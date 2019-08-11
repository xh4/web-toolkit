(in-package :live)



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



(defparameter *nav*
  (bs:nav
   (bs:nav-item
    (bs:nav-link (com:text "Link")))
   (bs:nav-item
    (bs:nav-link (com:text "Link")))
   (bs:nav-item
    (bs:nav-link :disabled-p t (com:text "Link")))))

(defparameter *button*
  (bs:button :style :danger
             :outline-p t
             :size :large
             :block-p t
             (com:text "foo")))

(defparameter *navbar*
  (bs:navbar
   (bs:navbar-brand
    "Web Toolkit")
   (bs:navbar-nav
    (bs:nav-item
     (bs:nav-link :href "#" "Hah")))))

(define-handler live-handler () ())

(defmethod handle ((handler live-handler) (request request))
  (setf (response-status *response*) 200)
  (setf (header-field *response* "Content-Type") "text/html")
  (setf (response-body *response*)
        (html:serialize
         (html:document
          (html:html
           (html:head
            (html:meta :charset "utf-8"))
           (html:body
            (html:h1 "Live")
            (html:script :src "/static/jquery-3.4.0.js")
            (html:script :src "/static/wt.live.js")
            (html:script :src "/static/index.js")))))))

(define-session live-session ())

(define-endpoint live-endpoint :session-class 'live-session)

(define-server live-server
    :handler (router
              (:get "/" live-handler)
              (:get "/live" live-endpoint)
              (:static :prefix "/static" :location #p"/home/xh/wt/live/"))
    :listeners (list
                (listener :port 8003)))

(defmethod on-message ((session live-session) message)
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

(defmethod on-open ((endpoint live-endpoint) session)
  (let* ((root *navbar*)
         (code (ps:ps*
                `(progn
                   (let ((link (ps:chain document (create-element "link"))))
                     (ps:chain link (set-attribute "rel" "stylesheet"))
                     (ps:chain link (set-attribute "type" "text/css"))
                     (ps:chain link (set-attribute "href" "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"))
                     (ps:chain document
                               (get-elements-by-tag-name "head")
                               0
                               (append-child link)))
                   (ps:chain ($ "body") (html ,(html:serialize (com:render root)))))))
         (message (list "eval" code)))
    (send-text session (json:encode-json message))))

(defmethod on-close ((endpoint live-endpoint) session &optional reason)
  (format t "Close: ~A~%" reason))

(defmethod on-error ((endpoint live-endpoint) session error)
  (format t "Error: ~A~%" error))

;; http://118.190.145.4:8003
