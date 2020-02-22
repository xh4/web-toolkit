(in-package :dom)

(defgeneric root (node))

(defgeneric parent (node))

(defgeneric children (node))

(defgeneric first-child (node))

(defgeneric last-child (node))

(defgeneric sibling (node))

(defgeneric previous-sibling (node))

(defgeneric next-sibling (node))

(defgeneric preceding (node))

(defgeneric following (node))

(defgeneric index (node))
