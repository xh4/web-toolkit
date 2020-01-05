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
           :uri-fragment
           :uri-string
           :uri-query-alist
           :uri-query-plist
           :uri-query-hash-table)
  (:import-from :split-sequence
                :split-sequence))
