(in-package :http-test)

(in-suite :http-test)

(test status
  (is (typep (status 200) 'status))
  (is (typep (status :ok) 'status))

  (is (equal 200 (status-code 200)))
  (is (equal 200 (status-code :ok)))
  (is (equal 200 (status-code (make-instance 'response :status 200))))
  (is (equal nil (status-code nil)))

  (is (equal :ok (status-keyword 200)))
  (is (equal :ok (status-keyword :ok)))
  (is (equal :ok (status-keyword (make-instance 'response :status 200))))
  (is (equal nil (status-keyword nil)))

  (is (equal "OK" (status-reason-phrase 200)))
  (is (equal "OK" (status-reason-phrase :ok)))
  (is (equal "OK" (status-reason-phrase (make-instance 'response :status 200))))
  (is (equal nil (status-reason-phrase nil))))
