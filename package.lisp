(in-package :cl-user)

(defpackage :wt.html
  (:use :cl :alexandria)
  (:import-from :serapeum
                :escape)
  (:export :html
           :html-string
           :*html-indent-size*
           :element-form-p
           :segment-element-form))

(defpackage :wt.form
  (:use :cl :alexandria :wt.html)
  (:import-from :metabang-bind
                :bind)
  (:import-from :optima
                :match)
  (:import-from :serapeum
                :true)
  (:export :define-form
           :render-form))

(defpackage :wt.list
  (:use :cl :alexandria :wt.html)
  (:import-from :closer-mop
                :class-precedence-list)
  (:export :list-group
           :list-segment
           :list-item
           :list-group-skeleton
           :list-group-search))

(defpackage :wt
  (:use :cl
        :alexandria
        :wt.html)
  (:import-from :ng
                :http-request
                :http-response)
  (:import-from :optima
                :match)
  (:import-from :serapeum
                :concat
                :string-prefix-p)
  (:import-from :cl-ppcre
                :regex-replace)
  (:import-from :cl-change-case
                :param-case :camel-case :snake-case :header-case)
  (:import-from :closer-mop
                :class-precedence-list)
  (:export :handler
           :define-handler
           :call-with-request
           :*request*
           :*response*))
