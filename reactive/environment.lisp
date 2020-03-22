(in-package :reactive)

(defvar *global-propagation* nil)

(defvar *local-propagation* nil)

(defvar *propagation-p* t)

(defvar *local-propagation-p* nil)

(defvar *record* nil)

(defmacro without-propagation (&body body)
  `(let ((*propagation-p* nil))
     ,@body))

(defmacro with-propagation (&body body)
  `(let ((*propagation-p* t))
     ,@body))
