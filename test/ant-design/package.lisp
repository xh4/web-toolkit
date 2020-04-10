(in-package :cl-user)

(defpackage :ant-design-test
  (:nicknames :antd-test :wt.ant-design-test :wt.antd-test)
  (:use :cl :antd :test :alexandria)
  (:export :run!))

(in-package :ant-design-test)
(def-suite :ant-design-test)
