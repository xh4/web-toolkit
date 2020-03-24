(in-package :documentation)

(define-page documentation-page ()
  ())

(defmethod page-title ((page documentation-page))
  "Lisp Web Toolkit")

(defmethod page-content ((page documentation-page))
  (documentation))

(define-server documentation-server ()
  ()
  (:listener (listener :port 7000))
  (:handler (router
             ;; (:get "/page.js" (lambda ()
             ;;                    (reply (merge-pathnames
             ;;                            "reactive/page.js"
             ;;                            (asdf:system-source-directory
             ;;                             (asdf:find-system :wt))))))
             (:page documentation-page))))

(defun start-server ()
  (http:start-server documentation-server))

(defun stop-server ()
  (http:stop-server documentation-server))
