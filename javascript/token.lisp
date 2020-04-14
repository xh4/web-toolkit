(in-package :javascript)

(defstruct token)

(defstruct (boolean-literal (:include token)))

(defstruct (identifier (:include token)))

(defstruct (keyword (:include token)))

(defstruct (null-literal (:include token)))

(defstruct (numeric-literal (:include token)))

(defstruct (punctuator (:include token)))

(defstruct (string-literal (:include token)))

(defstruct (regular-expression (:include token)))

(defstruct (template (:include token)))