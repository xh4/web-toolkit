(in-package :package)

(defclass human ()
  ((name
    :initarg :name
    :initform nil
    :reader human-name)
   (email
    :initarg :email
    :initform nil
    :reader human-email)
   (url
    :initarg :url
    :initform nil
    :reader human-url)))

(defmethod print-object ((human human) stream)
  (print-unreadable-object (human stream :type t)
    (with-slots (name email url) human
      (format stream "~A" name)
      (when email
        (format stream " <~A>" email))
      (when url
        (format stream " (~A)" url)))))

(defclass author (human) ())

(defclass maintainer (human) ())

(defclass contributor (human) ())
