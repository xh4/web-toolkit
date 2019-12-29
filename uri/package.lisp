(in-package :cl-user)

(defpackage :uri
  (:nicknames :wt.uri)
  (:use :cl :alexandria)
  (:export :uri
           )
  (:import-from :maxpc
                :=element
                :?satisfies
                :?eq
                :?test
                :%and
                :%or
                :%any
                :%some
                :?seq
                :?not
                :=subseq
                :%maybe
                :?end))
