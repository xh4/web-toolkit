(in-package :cl-user)

(defpackage :uri
  (:nicknames :wt.uri)
  (:use :cl :alexandria)
  (:export :uri
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment)
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
