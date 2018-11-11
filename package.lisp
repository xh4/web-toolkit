(in-package :cl-user)

(defpackage :wt.http
  (:use :cl :alexandria)
  (:import-from :wt.proto.http
                :request :response :response-headers-entry)
  (:export :request
           :request-local-address :request-local-port
           :request-remote-address :request-remote-port
           :request-scheme :request-method :request-path :request-query
           :request-version :request-headers :request-body
           :request-query-parameters :request-body-parameters
           :response
           :response-type :response-code :response-headers
           :response-body :response-path))

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
  (:import-from :optima
                :match)
  (:import-from :serapeum
                :concat
                :string-prefix-p)
  (:import-from :cl-ppcre
                :regex-replace)
  (:import-from :cl-change-case
                :param-case :camel-case :snake-case)
  (:import-from :closer-mop
                :class-precedence-list)
  (:import-from :lparallel
                :*kernel*
                :make-kernel
                :end-kernel
                :make-channel
                :invoke-transfer-error
                :submit-task
                :task-handler-bind)
  (:import-from :com.gigamonkeys.binary-data
                :read-value
                :write-value)
  (:import-from :com.gigamonkeys.binary-data.common-datatypes
                :u4)
  (:import-from :wt.http
                :request
                :request-scheme
                :request-method
                :request-path
                :request-query
                :request-version
                :request-headers
                :request-body
                :request-query-parameters
                :request-body-parameters
                :response
                :response-type
                :response-code
                :response-headers
                :response-body
                :response-path)
  (:export
   :*request*
   :*response*
   :define-listener
   :start-listener
   :stop-listener
   :define-handler))
