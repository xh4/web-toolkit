(in-package :live)

(progn
  (define-page test-page ()
    ())

  (com:define-component test-component ()
    ()
    (:render
     (lambda ()
       (html:h1 "Test Page 3"))))

  (defmethod page-content ((page test-page))
    (test-component))

  (http:define-server test-server ()
    ()
    (:listener (http:listener :port 4001))
    (:handler (router
               (:page test-page)))))

;; (http:start-server test-server)
;; (http:stop-server test-server)
