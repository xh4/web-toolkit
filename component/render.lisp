(in-package :component)

(defgeneric render (component))

(defmethod render ((element html:element))
  element)

(defmethod render ((text html:text))
  text)

(defmacro define-render ())
