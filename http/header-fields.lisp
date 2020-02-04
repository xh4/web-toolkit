(in-package :http)

(defclass general-header-field (header-field)
  ())

(defclass request-header-field (header-field)
  ())

(defclass response-header-field (header-field)
  ())

(defclass entity-header-field (header-field)
  ())
