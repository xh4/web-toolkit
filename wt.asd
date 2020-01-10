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
               :wt.component
               :wt.form
               :wt.sql)
  :serial t
  :in-order-to ((test-op (test-op :wt/test))))

(defsystem wt/test
  :depends-on (:wt.html/test
               :wt.json/test
               :wt.uri/test
               :wt.http/test
               :wt.websocket/test
               :wt.component/test
               :wt.form/test
               :wt.sql/test)
  :perform (test-op (o s)
                    (uiop:eval-input "(setf fiveam:*on-error* :debug)")
                    (uiop:eval-input "(setf fiveam:*on-failure* :debug)")
                    (uiop:symbol-call :fiveam :run! :html-test)
                    (uiop:symbol-call :fiveam :run! :json-test)
                    (uiop:symbol-call :fiveam :run! :uri-test)
                    (uiop:symbol-call :fiveam :run! :http-test)
                    (uiop:symbol-call :fiveam :run! :websocket-test)
                    (uiop:symbol-call :fiveam :run! :component-test)
                    (uiop:symbol-call :fiveam :run! :form-test)
                    (uiop:symbol-call :fiveam :run! :sql-test)))
