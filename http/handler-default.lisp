(in-package :http)

(define-handler default-handler ()
  ())

(defmethod handle ((handler default-handler) request)
  (reply
   (html:document
    (html:html
     (html:head
      (html:title "Lisp Web Toolkit"))
     (html:body
      (html:h1 "Lisp Web Toolkit"))))))
