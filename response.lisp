(in-package :wt)

(defgeneric response-status (response))

(defgeneric (setf response-status) (value response))

(defgeneric response-header (response))

(defgeneric (setf response-header) (value response))

(defgeneric response-header-field (response name))

(defgeneric (setf response-header-field) (value response name))

(defgeneric response-body (response))

(defgeneric (setf response-body) (value response))
