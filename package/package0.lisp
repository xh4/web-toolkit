(in-package :cl-user)

(defpackage :package
  (:nicknames :pkg)
  (:use :cl :alexandria)
  (:shadow :package
           :package-name))
