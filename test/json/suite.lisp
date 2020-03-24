(in-package :json-test)

(in-suite :json-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pending-tests*
    '(NUMBER-REAL-FRACTION-EXPONENT
      NUMBER-REAL-EXPONENT
      NUMBER)))

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

;; (decode "[123e45]")
;; (decode "[123.456e78]")
;; (decode "[123e65]")
