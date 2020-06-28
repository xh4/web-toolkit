(in-package :http)

(defclass cookie ()
  ((name
    :initarg :name
    :initform nil
    :accessor cookie-name)
   (value
    :initarg :value
    :initform nil
    :accessor cookie-value)
   (domain
    :initarg :domain
    :initform nil
    :accessor cookie-domain)
   (path
    :initarg :path
    :initform nil
    :accessor cookie-path)
   (expires
    :initarg :expires
    :initform nil
    :accessor cookie-expires)
   (max-age
    :initarg :max-age
    :initform nil
    :accessor cookie-max-age)
   (secure
    :initarg :secure
    :initform nil
    :accessor cookie-secure)
   (http-only
    :initarg :http-only
    :initform nil
    :accessor cookie-http-only)))

(defmacro cookie (&key name value domain path expires max-age secure http-only)
  `(make-instance 'cookie
                  :name ,name
                  :value ,value
                  :domain ,domain
                  :path ,path
                  :expires ,expires
                  :max-age ,max-age
                  :secure ,secure
                  :http-only ,http-only))

;; TODO: Check cookie name
;; TODO: Check cookie value
;; TODO: Check cookie attributes
(defmethod intialize-instance :after ((cookie cookie))
  (check-type (cookie-name cookie) string)
  (assert (plusp (length (cookie-name cookie))))
  (check-type (cookie-value cookie) string)
  (assert (plusp (length (cookie-value cookie))))
  (check-type (cookie-domain cookie) (or string null))
  (check-type (cookie-path cookie) (or string null))
  (check-type (cookie-expires cookie) (or string null))
  (check-type (cookie-max-age cookie) (or integer null)))

(defmethod print-object ((cookie cookie) stream)
  (print-unreadable-object (cookie stream :type t :identity t)
    (let ((name (cookie-name cookie))
          (value (cookie-value cookie))
          (domain (cookie-domain cookie))
          (path (cookie-path cookie))
          (expires (cookie-expires cookie))
          (max-age (cookie-max-age cookie))
          (secure (cookie-secure cookie))
          (http-only (cookie-http-only cookie)))
      (flet ((write-attribute-0 (name value)
               (when value
                 (write-string "; " stream)
                 (write-string name stream)))
             (write-attribute-1 (name value)
               (when value
                 (write-string "; " stream)
                 (write-string name stream)
                 (write-char #\= stream)
                 (format stream "~A" value))))
        (format stream "~A=~A" name value)
        (write-attribute-1 "Domain" domain)
        (write-attribute-1 "Path" path)
        (write-attribute-1 "Expires" expires)
        (write-attribute-1 "Max-Age" max-age)
        (write-attribute-0 "Secure" secure)
        (write-attribute-0 "HttpOnly" http-only)))))
