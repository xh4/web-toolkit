(in-package :html)

(defclass text (dom:text) ())

(defmethod print-object ((text text) stream)
  (print-unreadable-object (text stream :type t :identity t)
    (let ((data (dom:data text)))
      (when data
        (format stream "~S" data)))))

(defun text (&optional data)
  (check-type data (or null string))
  (make-instance 'text :data data))
