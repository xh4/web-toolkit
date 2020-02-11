(in-package :http-test)

(in-suite :http-test)

(test status
  (is (typep (status 200) 'status))

  (is (typep (status :ok) 'status)))
