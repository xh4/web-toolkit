(in-package :bootstrap)

(define-component navbar ()
  ())

(defmethod render ((navbar navbar))
  (html:nav :class "navbar"
            (mapcar 'render (children navbar))))

(define-component navbar-brand ()
  ())

(defmethod render ((navbar-brand navbar-brand))
  (html:span :class "navbar-brand"
             (mapcar 'render (children navbar-brand))))

(define-component navbar-text ()
  ())

(define-component navbar-nav (nav) ())
