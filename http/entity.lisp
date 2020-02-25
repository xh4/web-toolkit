(in-package :http)

(defclass entity (request response) ())

(defgeneric entity-header (entity)
  (:method ((entity entity))
    (slot-value entity 'header)))

(defgeneric entity-body (entity)
  (:method ((entity entity))
    (slot-value entity 'body)))

(defgeneric entity-status (entity)
  (:method ((entity entity))
    (slot-value entity 'status)))

(defgeneric entity-method (entity)
  (:method ((entity entity))
    (slot-value entity 'method)))

(defgeneric entity-uri (entity)
  (:method ((entity entity))
    (slot-value entity 'uri)))

(defgeneric entity-version (entity)
  (:method ((entity entity))
    (slot-value entity 'version)))

(defgeneric allow (entity))

(defgeneric content-encoding (entity))

(defgeneric content-language (entity))

(defgeneric content-length (entity))

(defgeneric content-location (entity))

(defgeneric content-md5 (entity))

(defgeneric content-range (entity))

(defgeneric content-type (entity))

(defgeneric expires (entity))

(defgeneric last-modified (entity))
