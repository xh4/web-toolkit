(in-package :form)

(define-form test-form ()
  ((name
    :type :text
    :required t
    :autofocus t
    :label "姓名")
   (email
    :type :email
    :required t
    :label "电子邮箱")
   (password
    :type :password
    :required t
    :label "密码")))

(defmethod process-form ((form test-form))
  (http:redirect :back :status 303))

(define-render test-form ()
  (let ((form (call-next-method)))
    (dom:insert-before form (html:h1 "My Form") (dom:first-child form))
    (dom:append-child form (html:input :type "submit"))
    form))

(http:define-server test-server
    :handler (http:router
              (:form test-form))
    :listeners (list
                (http:listener :port 8005)))

;; (http:start-server test-server)
