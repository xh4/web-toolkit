(in-package :dom)

(defclass comment (character-data)
  ())

(defmethod node-type ((comment comment))
  comment-node)

(defmethod node-name ((comment comment))
  "#comment")

(defmethod node-value ((comment comment))
  (data comment))

(defmethod text-content ((comment comment))
  (data comment))

(defun create-comment (document data)
  )
