;;;; -*- Mode: LISP -*-

(defsystem wt
  :version "3.1415"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :description "Object-oriented Lisp Systems for Rapid Web Application Development"
  :license "BSD 3-Clause"
  :depends-on (:wt.utility
               :wt.uri
               :wt.dom
               :wt.html
               :wt.css
               ;; :wt.component
               :wt.json
               :wt.http
               :wt.websocket)
  :in-order-to ((test-op (test-op :wt/test))))

(defsystem wt/test
  :depends-on (:wt.utility/test
               :wt.uri/test
               :wt.dom/test
               :wt.html/test
               :wt.css/test
               ;; :wt.component/test
               :wt.json/test
               :wt.http/test
               :wt.websocket/test)
  :perform (test-op (o s)
                    (uiop:eval-input "(setf test:*on-error* :debug)")
                    (uiop:eval-input "(setf test:*on-failure* :debug)")
                    (uiop:symbol-call :test :run! :uri-test)
                    (uiop:symbol-call :test :run! :dom-test)
                    (uiop:symbol-call :test :run! :html-test)
                    (uiop:symbol-call :test :run! :css-test)
                    ;; (uiop:symbol-call :test :run! :component-test)
                    (uiop:symbol-call :test :run! :json-test)
                    (uiop:symbol-call :test :run! :http-test)
                    (uiop:symbol-call :test :run! :websocket-test)))
