(in-package :http)

(define-handler default-handler ()
  ()
  (:function (lambda (request)
               (declare (ignore request))
               (reply
                (html:document
                 (html:html
                  (html:head
                   (html:title "Lisp Web Toolkit"))
                  (html:body
                   (html:h1 "Lisp Web Toolkit"))))))))
