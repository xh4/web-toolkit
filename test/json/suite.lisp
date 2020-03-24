(in-package :json-test)

(in-suite :json-test)

(defparameter *pending-tests*
  '(NUMBER-REAL-FRACTION-EXPONENT
    NUMBER-REAL-EXPONENT
    NUMBER
    STRUCTURE-OPEN-ARRAY-OBJECT
    STRUCTURE-100000-OPENING-ARRAYS))

(defmacro test-parsing ()
  (let ((pathnames (directory
                    (merge-pathnames
                     "test/json/test_parsing/*.json"
                     (asdf:system-source-directory
                      (asdf:find-system :wt))))))
    `(progn
       ,@(loop for pathname in pathnames
            for name = (pathname-name pathname)
            for expect-success = (eq #\y (char name 0))
            for expect-fail = (eq #\n (char name 0))
            for test-name = (intern (cl-ppcre:regex-replace-all "_" (string-upcase (subseq name 2)) "-"))
            collect
              `(test ,test-name
                 ,@(unless (find test-name *pending-tests*)
                     (cond
                       (expect-success
                        `((it
                            (finishes (json:decode ,pathname)))))
                       (expect-fail
                        `((it
                            (signals error (json:decode ,pathname))))))))))))

(test-parsing)
