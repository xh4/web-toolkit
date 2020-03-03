(in-package :cl-user)

(defpackage :style
  (:nicknames :wt.style)
  (:use :cl :alexandria)
  (:import-from :closer-mop
                :validate-superclass))
