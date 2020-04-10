(in-package :live)

(define-page test-page () ())

(com:define-component test-component ()
  ((colors
    :initform '(orange blue)))
  (:render
   (lambda (c)
     (with-slots (colors) c
       (html:div
        (loop for color in colors
           for style = (format nil "background: ~(~A~); padding: 20px; margin: 20px;" color)
           collect (html:div :style style))))))
  (:style
   (lambda ()
     (css:rule ".test-component"
               (css:border "5px solid red")))))

(defmethod page-content ((page test-page))
  (test-component))

(http:define-server test-server ()
  ()
  (:listener (http:listener :port 4001))
  (:handler (router
             (:page test-page))))

;; (http:start-server test-server)
;; (http:stop-server test-server)
