(in-package :uri)

(defmacro define-uri-accessor (component)
  (let ((accessor-name (intern (format nil "URI-~A" (symbol-name component))))
        (checker-name (intern (format nil "CHECK-~A" (symbol-name component)))))
    `(progn
       (defgeneric ,accessor-name (uri)
         (:method ((uri string))
           (,accessor-name (uri uri)))
         (:method ((uri uri))
           (slot-value uri ',component)))
       (defgeneric (setf ,accessor-name) (value uri)
         (:method (value (uri uri))
           (,checker-name value)
           (setf (uri-string uri) nil)
           (setf (slot-value uri ',component) value))))))

(define-uri-accessor userinfo)

(define-uri-accessor host)

(define-uri-accessor port)

(define-uri-accessor path)

(define-uri-accessor query)

(define-uri-accessor fragment)
