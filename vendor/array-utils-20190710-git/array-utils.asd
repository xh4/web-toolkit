#|
 This file is a part of Array-Utils
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem array-utils
  :name "Array-Utils"
  :version "1.1.1"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A few utilities for working with arrays."
  :homepage "https://Shinmera.github.io/array-utils/"
  :bug-tracker "https://github.com/Shinmera/array-utils/issues"
  :source-control (:git "https://github.com/Shinmera/array-utils.git")
  :serial T
  :components ((:file "utils"))
  :depends-on ()
  :in-order-to ((asdf:test-op (asdf:test-op :array-utils-test))))
