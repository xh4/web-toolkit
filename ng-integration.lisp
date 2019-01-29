(in-package :wt)

;; TODO: Translate status value?
(defmethod response-status ((response ng:http-response))
  (ng:http-response-status response))

(defmethod (setf response-status) ((value integer) (response ng:http-response))
  (setf (ng:http-response-status response) value))

;; TODO: Translate status value
(defmethod (setf response-status) ((value symbol) (response ng:http-response))
  (setf (ng:http-response-status response) 200))

(defmethod response-header ((response ng:http-response))
  (ng:http-response-header response))

(defmethod (setf response-header) ((response ng:http-response) value)
  (setf (ng:http-response-header response) value))

(defmethod response-header-field ((response ng:http-response) (name symbol))
  (response-header-field response (symbol-name name)))

(defmethod response-header-field ((response ng:http-response) (name string))
  (cdr (assoc name (ng:http-response-header response) :test 'string-equal)))

(defmethod (setf response-header-field) ((value string) (response ng:http-response) (name symbol))
  (setf (response-header-field response (header-case (symbol-name name))) value))

(defmethod (setf response-header-field) ((value string) (response ng:http-response) (name string))
  (appendf (ng:http-response-header response) (list (cons name value))))

(defmethod response-body ((response ng:http-response))
  (ng:http-response-body response))

(defmethod (setf response-body) ((value string) (response ng:http-response))
  (setf (ng:http-response-body response)
        (flex:string-to-octets value :external-format :utf-8)))

(defmethod (setf response-body) ((value simple-array) (response ng:http-response))
  (setf (ng:http-response-body response) value))
