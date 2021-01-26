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

(defclass processing-instruction (character-data)
  ())

(defmethod child-nodes ((character-data character-data))
  (make-instance 'node-list :nodes #()))

(defun create-text-node (document data)
  (check-type data string)
  (make-instance 'text :document document :data data))

(defmethod node-type ((processing-instruction processing-instruction))
  processing-instruction-node)

(defmethod node-type ((text text))
  text-node)

(defmethod node-name ((text text))
  "#text")

(defmethod node-value ((text text))
  (data text))

(defmethod text-content ((text text))
  (data text))

(defmethod node-value ((processing-instruction processing-instruction))
  (data processing-instruction))

(defmethod text-content ((processing-instruction processing-instruction))
  (data processing-instruction))
