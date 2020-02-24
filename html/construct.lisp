(in-package :html)

(defclass constructor () ())

(defmethod print-object ((constructor constructor) stream)
  (print-unreadable-object (constructor stream :type t :identity t)))

(defgeneric construct (constructor &rest arguments))

(defun constructor (tag-name)
  (let ((symbol (typecase tag-name
                  (symbol (find-symbol (symbol-name tag-name) :html))
                  (string (find-symbol (string-upcase tag-name) :html)))))
    (when (and (boundp symbol) (typep (symbol-value symbol) 'constructor))
      (symbol-value symbol))))
