(in-package :live)

(define-page test-page ()
  ())

(com:define-component test-component ()
  ()
  (:render
   (lambda ()
     (html:div :style "color: red"
              :class "foo bar"
              "Test Page 1"
              (html:ul
               (html:li "000")
               (html:li "111")
               (html:li "222")
               (html:li "333"))))))

(defmethod page-content ((page test-page))
  (test-component))

(http:define-server test-server ()
  ()
  (:listener (http:listener :port 4001))
  (:handler (router
             (:page test-page))))

;; (http:start-server test-server)
;; (http:stop-server test-server)
