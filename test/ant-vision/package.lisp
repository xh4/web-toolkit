(in-package :cl-user)

(defpackage :ant-vision-test
  (:nicknames :antv-test :wt.ant-vision-test :wt.antv-test)
  (:use :cl :antv :test :alexandria)
  (:export :run!))

(in-package :ant-vision-test)
(def-suite :ant-vision-test)
