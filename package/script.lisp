(in-package :package)

(defclass script ()
  ((name
    :initarg :name
    :initform nil
    :reader script-name)
   (command
    :initarg :command
    :initform nil
    :reader script-command)))

(defmethod print-object ((script script) stream)
  (print-unreadable-object (script stream :type t)
    (format stream "~A" (script-name script))))
