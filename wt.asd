;;;; -*- Mode: LISP -*-

(defsystem wt
  :version "3.1415"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.html
               :wt.json
               :wt.uri
               :wt.http
               :wt.websocket
               :wt.component)
  :serial t
  :in-order-to ((test-op (test-op :wt/test))))

(defsystem wt/test
  :depends-on (:wt.html/test
               :wt.json/test
               :wt.uri/test
               :wt.http/test
               :wt.websocket/test
               :wt.component/test)
  :serial t
  :perform (test-op (o s)
                    (uiop:eval-input "(setf test:*on-error* :debug)")
                    (uiop:eval-input "(setf test:*on-failure* :debug)")
                    (uiop:symbol-call :test :run! :html-test)
                    (uiop:symbol-call :test :run! :json-test)
                    (uiop:symbol-call :test :run! :uri-test)
                    (uiop:symbol-call :test :run! :http-test)
                    (uiop:symbol-call :test :run! :websocket-test)
                    (uiop:symbol-call :test :run! :component-test)))
