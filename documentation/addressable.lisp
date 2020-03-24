(in-package :documentation)

(defclass addressable ()
  ((no
    :initarg :no
    :initform nil
    :accessor no)
   (title
    :initarg :title
    :initform nil
    :accessor title)
   (id
    :initarg :id
    :initform nil
    :accessor id)))

(defun title-id (title)
  (cl-ppcre:regex-replace-all "\\s+" (format nil "~(~A~)" title) "-"))

(defmethod initialize-instance :after ((addressable addressable) &key)
  (setf (id addressable) (title-id (title addressable))))

(defmethod id ((nothing null)) "")
