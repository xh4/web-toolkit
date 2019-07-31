(in-package :cl-user)

(defpackage :http
  (:nicknames :wt.http)
  (:use :cl :alexandria)
  (:import-from :closer-mop
                :compute-class-precedence-list
                :subclassp)
  (:import-from :cl-change-case
                :header-case))
