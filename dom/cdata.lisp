(in-package :dom)

(defclass cdata-section (text)
  ())

(defmethod node-type ((cdata-section cdata-section))
  cdata-section-node)

(defmethod node-name ((cdata-section cdata-section))
  "#cdata-section")

(defun create-cdata-section (document data)
  )
