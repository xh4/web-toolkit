(in-package :bootstrap)

(define-component nav ()
  ())

(defmethod render ((nav nav))
  (html:ul :class "nav"
           (mapcar 'render (children nav))))

(define-component nav-item ()
  ())

(defmethod render ((nav-item nav-item))
  (html:li :class "nav-item"
           (mapcar 'render (children nav-item))))

(define-component nav-link ()
  ((href
    :initarg :href
    :initform nil
    :accessor nav-link-href)
   (disabled-p
    :initarg :disabled-p
    :initform nil
    :accessor nav-link-disabled-p)))

(defmethod render ((nav-link nav-link))
  (with-slots (disabled-p) nav-link
    (eval
     `(html:a :class ,(format nil "nav-link ~:[~;disabled~]" disabled-p)
              :href ,(nav-link-href nav-link)
              ,@(when disabled-p
                  `(:tabindex "-1"
                              :aria-disabled "true"))
              ,@(mapcar 'render (children nav-link))))))
