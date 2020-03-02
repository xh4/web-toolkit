;;;; -*- Mode: LISP -*-

(defsystem wt
  :version "3.1415"
  :author "Xiangyu He"
  :mailto "xh@coobii.com"
  :depends-on (:wt.uri
               :wt.dom
               :wt.html
               ;; :wt.component
               ;; :wt.style
               :wt.json
               :wt.http
               :wt.websocket)
  :in-order-to ((test-op (test-op :wt/test))))

(defsystem wt/test
  :depends-on (:wt.uri/test
               :wt.dom/test
               :wt.html/test
               ;; :wt.component/test
               ;; :wt.style/test
               :wt.json/test
               :wt.http/test
               :wt.websocket/test)
  :perform (test-op (o s)
                    (uiop:eval-input "(setf test:*on-error* :debug)")
                    (uiop:eval-input "(setf test:*on-failure* :debug)")
                    (uiop:symbol-call :test :run! :uri-test)
                    (uiop:symbol-call :test :run! :dom-test)
                    (uiop:symbol-call :test :run! :html-test)
                    ;; (uiop:symbol-call :test :run! :component-test)
                    ;; (uiop:symbol-call :test :run! :style-test)
                    (uiop:symbol-call :test :run! :json-test)
                    (uiop:symbol-call :test :run! :http-test)
                    (uiop:symbol-call :test :run! :websocket-test)))
