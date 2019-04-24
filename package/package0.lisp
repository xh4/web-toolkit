(in-package :cl-user)

(defpackage :package
  (:nicknames :wt.package :wt.pkg :pkg)
  (:use :cl :alexandria)
  (:shadow :package
           :package-name))

(defpackage :packages
  (:nicknames :wt.packages :wt.pkgs :pkgs))
