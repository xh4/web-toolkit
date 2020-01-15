(in-package :json-test)

(in-suite :json-test)

;; Test decoder

(test json-literal
  (is-true (decode "  true"))
  (is-true (decode "  true "))
  (is-true (decode "true "))
  (is-true (decode "true"))
  (signals error (decode "trUe "))
  (is-false (decode "false"))
  (is-false (decode "null")))

(test json-string
  (is (string= "hello"
              (decode "  \"hello\"")))
  (is (string= "new-line
returned!"
               (decode "\"new-line\\nreturned!\"")))
  (is (string= (make-string 1 :initial-element (code-char (+ (* 10 16) 11)))
               (decode "  \"\\u00ab\""))))

(test json-array
  (is (equalp
       '("hello" "hej" "ciao")
       (decode " [ \"hello\",  \"hej\",
                   \"ciao\" ]")))
  (is (equalp '(1 2 3)
              (decode "[1,2,3]")))
  (is (equalp '(t nil nil)
              (decode "[true,null,false]")))
  (is (equalp '()
              (decode "[]"))))

(test json-object
  (let ((input " { \"hello\" : \"hej\" ,
                       \"hi\" : \"tjena\" ,
                       \"start_XPos\" : 98
                  }"))
    ;; (is (equalp '(("hello" . "hej") ("hi" . "tjena") ("start_XPos" . 98))
    ;;             (decode input)))
    ;; (is-false (decode " {  } "))
    ;; (is-false (decode "{}"))
    ))

(defmacro with-fp-overflow-handler (handler-expr &body body)
  (let ((err (gensym)))
    `(handler-bind ((floating-point-overflow
                     (lambda (,err)
                       (declare (ignore ,err))
                       ,handler-expr)))
       ,@body)))

(defmacro with-no-char-handler (handler-expr &body body)
  (let ((err (gensym)))
    `(handler-bind ((json::no-char-for-code
                     (lambda (,err)
                       (declare (ignore ,err))
                       ,handler-expr)))
       ,@body)))

(test json-number
  (is (= 100 (decode "100")))
  (is (= 10.01 (decode "10.01")))
  (is (= -2.3 (decode "-2.3")))
  (is (= -2.3e3 (decode "-2.3e3")))
  (is (= -3e4 (decode "-3e4")))
  (is (= 3e4 (decode "3e4")))
  (let ((*read-default-float-format* 'double-float))
    (is (= 2d40 (decode "2e40"))))
  #-(or (and sbcl darwin) (and allegro macosx))
  (is (equalp "BIG:2e444"
              (with-fp-overflow-handler
                  (invoke-restart 'bignumber-string "BIG:")
                (decode "2e444"))))
  #-(or (and sbcl darwin) (and allegro macosx))
  (is (= (* 2 (expt 10 444))
         (with-fp-overflow-handler
             (invoke-restart 'rational-approximation)
           (decode "2e444"))))
  ;; In SBCL on Darwin, constructing the float from parts by explicit
  ;; operations yields #.SB-EXT:SINGLE-FLOAT-POSITIVE-INFINITY.
  #+(and sbcl darwin)
  (is (= (* 2.0 (expt 10.0 444))
         (decode "2e444"))))


(defparameter *json-test-files-path*
  (asdf:system-relative-pathname "wt.json/test" "test/json/"))

(defun test-file (name)
  (make-pathname :name name :type "json" :defaults *json-test-files-path*))

(defun decode-file (path)
  (with-open-file (stream path :direction :input)
    (with-fp-overflow-handler (invoke-restart 'placeholder :infty)
      (with-no-char-handler (invoke-restart 'substitute-char #\?)
        (json::decode-json-strict stream)))))

;; All test files are taken from http://www.crockford.com/JSON/JSON_checker/test/

(test pass-1
  (decode-file (test-file "pass1")))

(test pass-2
  (decode-file (test-file "pass2")))

(test pass-3
  (decode-file (test-file "pass3")))

(defparameter *ignore-tests* '(
  1 ; says: "A JSON payload should be an object or array, not a string.", but who cares?
  7 ; says: ["Comma after the close"],  ,but decode-file stops parsing after one object has been retrieved
  8 ; says ["Extra close"]] ,but decode-file stops parsing after one object has been retrieved
  10; says {"Extra value after close": true} "misplaced quoted value", but
    ;   decode-file stops parsing after one object has been retrieved
  18; says [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]], but there is no formal limit
))

(defparameter *ignore-tests-strict* '(
  18; says [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]], but there is no formal limit
))

(test fail-files
  (dotimes (x 24)
    (if (member x *ignore-tests-strict*)
        (is-true t)
        (5am:signals error
          (decode-file (test-file (format nil "fail~a" x)))))))

(defun contents-of-file(file)
  (with-open-file (stream file :direction :input)
     (let ((s (make-string (file-length stream))))
      (read-sequence s stream)
      s)))

(test non-strict-json
   (let ((not-strictly-valid "\"right\\'s of man\""))
     (5am:signals json::json-syntax-error
       (decode not-strictly-valid))
     (let ((json::*use-strict-json-rules* nil))
       (declare (special json::*use-strict-json-rules*))
       (is (string= (decode not-strictly-valid)
                    "right's of man")))))
