(in-package :dom)

(defclass character-data (node
                          non-document-type-child-node
                          child-node)
  ((data
    :initarg :data
    :initform nil
    :reader data)))

(defclass text (character-data)
  ())

(defmethod children ((text text)))