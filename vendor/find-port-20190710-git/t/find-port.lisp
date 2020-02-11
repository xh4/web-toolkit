(in-package :cl-user)
(defpackage find-port-test
  (:use :cl :fiveam))
(in-package :find-port-test)

(def-suite tests
  :description "find-port tests.")
(in-suite tests)

(test find-ports
  (finishes
    (find-port:find-port))
  (let ((port (find-port:find-port)))
    (is
     (integerp port))
    (is
     (find-port:port-open-p port))))

(run! 'tests)
