(in-package :documentation)

(define-page documentation-page () ())

(defmethod page-title ((page documentation-page))
  "Lisp Web Toolkit")

(defmethod page-content ((page documentation-page))
  (documentation
   chapter-get-started
   chapter-uri
   chapter-html
   chapter-json
   chapter-css
   chapter-http
   chapter-websocket))

(define-server documentation-server ()
  ()
  (:listener (listener :port 7000))
  (:handler (router
             (:page documentation-page))))

(defun start-server ()
  (http:start-server documentation-server))

(defun stop-server ()
  (http:stop-server documentation-server))
