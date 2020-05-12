(in-package :html)

(defclass text (dom:text)
  ((data-stream
    :initarg :data-stream
    :initform nil)))

(defmethod print-object ((text text) stream)
  (print-unreadable-object (text stream :type t :identity t)
    ;; TODO: trim text data
    (format stream "~S" (slot-value text 'dom:data))))

(defun text (&optional data)
  (check-type data (or null string))
  (make-instance 'text :data data))
