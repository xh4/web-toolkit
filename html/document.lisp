(in-package :html)

(defclass document (dom:document) ())

(defgeneric document-title (document))

(defgeneric document-body (document))

(defgeneric document-head (document))

(defgeneric document-images (document))

(defgeneric document-links (document))

(defgeneric document-forms (document))

(defgeneric document-scripts (document))

(defclass document-constructor (constructor) ())

(defun document (&optional child)
  (check-type child (or html null))
  (let ((document (make-instance 'document :children (when child (list child)))))
    document))
