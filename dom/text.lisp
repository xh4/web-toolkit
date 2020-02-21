(in-package :dom2)

(defclass character-data (node
                          non-document-type-child-node
                          child-node)
  ((value
    :initarg :data
    :reader data)))

(defclass text (character-data)
  ())

(defclass comment (character-data)
  ())
