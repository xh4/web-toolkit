(in-package :wt)

(defgeneric request-local-address (request))

(defgeneric request-local-port (request))

(defgeneric request-remote-address (request))

(defgeneric request-remote-port (request))

(defgeneric request-scheme (request))

(defgeneric request-method (request))

(defgeneric request-path (request))

(defgeneric request-query (request))

(defgeneric request-version (request))

(defgeneric request-header (request))

(defgeneric request-header-field (request))

(defgeneric request-body (request))

(defgeneric reqeust-query-parameters (request))

(defgeneric request-urlencoded-body-parameters (request))

(defgeneric request-multipart-body-parameters (request))

(defgeneric request-body-parameters (request))

(defgeneric request-parameters (request))
