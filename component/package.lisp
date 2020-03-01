(in-package :cl-user)

(defpackage :component
  (:nicknames :com :wt.com :wt.component)
  (:use :cl :alexandria :utility)
  (:shadow :variable)
  #+sb-package-locks
  (:lock t)
  (:export :id
           :define-component
           :render
           :serialize
           :root
           :children
           :append-child
           :define-variable
           :variable)
  (:import-from :html
                :append-child
                :serialize
                :root
                :children)
  (:import-from :closer-mop
                :allocate-instance
                :validate-superclass
                :class-slots
                :slot-definition-name
                :compute-class-precedence-list)
  (:import-from :split-sequence
                :split-sequence))
