(in-package :cl-user)

(defpackage :uri
  (:nicknames :wt.uri)
  (:use :cl :alexandria :wt.utility)
  #+sb-package-locks
  (:lock t)
  (:export :uri
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment
           :uri-string
           :merge-uri
           :percent-encode
           :percent-decode)
  (:import-from :split-sequence
                :split-sequence))
