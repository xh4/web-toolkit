#|
 This file is a part of lQuery
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem array-utils-test
  :version "1.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Tests for the array-utils package"
  :components ((:file "tests"))
  :depends-on (:array-utils :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :array-utils-test)))
