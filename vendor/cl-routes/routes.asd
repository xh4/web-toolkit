;;;; routes.asd
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:routes-system
  (:use #:cl #:asdf))

(in-package #:routes-system)

(defsystem routes
  :depends-on (#:quri #:iterate #:split-sequence)
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "uri-template" :depends-on ("package"))
                                     (:file "route" :depends-on ("uri-template"))
                                     (:file "mapper" :depends-on ("route"))
                                     #+swank (:file "routes-swank" :depends-on ("mapper"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'routes))))
  (operate 'load-op 'routes-test)
  (operate 'test-op 'routes-test :force t))

(defsystem routes-test
  :depends-on (#:routes #:lift)
  :components ((:module "t"
                        :components ((:file "core")
                                     (:file "mapper" :depends-on ("core"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'routes-test))))
  (operate 'load-op 'routes-test )
  (let* ((test-results (funcall (read-from-string "routes.test:run-routes-tests")))
         (errors (funcall (read-from-string "lift:errors")  test-results))
         (failures (funcall (read-from-string "lift:failures") test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))
