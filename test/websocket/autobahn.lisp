(in-package :websocket-test)

(defparameter *wstest-executable-path* "C:\\Python27amd64\\Scripts\\wstest.exe")

(defparameter *wstest-port* 54321)

(defparameter *wstest-complete* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *wstest-cases* nil))

(defun make-test-spec (&key url outdir cases)
  (uiop:with-temporary-file (:stream stream :pathname pathname
                                     :keep t
                                     :prefix "spec"
                                     :type "json" )
    (json:encode
     (json:object
      "servers" (list
                 (json:object
                  "agent" "WT.WebSocket"
                  "url" url))
      "outdir" outdir
      "cases" cases)
     stream)
    pathname))

(define-session test-session () ()
                (:on-message (lambda (session message)
                               (typecase message
                                 (string (send-text session message))
                                 (t (send-binary session message))))))

(define-endpoint test-endpoint () ()
                 (:session-class 'test-session))

(http:define-server test-server
    :handler (http:router
              (:get "/" test-endpoint))
    :listeners (list
                (http:listener :port 4000)))

(defmacro with-test-server (port &body body)
  `(unwind-protect
        (progn
          (let ((listener (first (http::server-listeners test-server))))
            (setf (http::listener-port listener) ,port))
          (http:start-server test-server)
          ,@body)
     (http:stop-server test-server)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun id-string (id &optional (delimiter #\.))
    (typecase id
      (string (id-string (id-list id) delimiter))
      (list (let ((o ""))
              (loop for part in id
                 if (= (length o) 0)
                 do (setf o (format nil "~A" part))
                 else
                 do (setf o (concatenate 'string o (format nil "~A~A" delimiter part)))
                 finally (return o))))))
  (defun id-list (id)
    (typecase id
      (list id)
      (string (mapcar 'parse-integer
                      (split-sequence:split-sequence #\. id))))))

(defclass test-case ()
  ((id
    :initarg :id
    :initform nil
    :accessor test-case-id)
   (agent
    :initarg :agent
    :initform nil
    :accessor test-case-agent)
   (status
    :initarg :status
    :initform nil
    :accessor test-case-status)
   (duration
    :initarg :duration
    :initform nil
    :accessor test-case-duration)
   (time
    :initarg :time
    :initform nil
    :accessor test-case-time)
   (description
    :initarg :description
    :initform nil
    :accessor test-case-description)
   (expectation
    :initarg :expectation
    :initform nil
    :accessor test-case-expectation)
   (expected
    :initarg :expected
    :initform nil
    :accessor test-case-expected)
   (received
    :initarg :received
    :initform nil
    :accessor test-case-received)
   (closing-behavior
    :initarg :closing-behavior
    :initform nil
    :accessor test-case-closing-behavior)
   (closing-status
    :initarg :closing-status
    :initform nil
    :accessor test-case-closing-status)
   (opening-handshake-request
    :initarg :opening-handshake-request
    :initform nil
    :accessor test-case-opening-handshake-request)
   (opening-handshake-response
    :initarg :opening-handshake-response
    :initform nil
    :accessor test-case-opening-handshake-response)))

(defun read-test-case-report (pathname)
  (let ((report (json:decode pathname)))
    (make-instance 'test-case
                   :id (json:get report "id")
                   :agent (json:get report "agent")
                   :status (switch ((json:get report "behavior") :test 'equal)
                             ("FAILED" :fail)
                             ("OK" :pass))
                   :duration (json:get report "duration")
                   :description (json:get report "description")
                   :expectation (json:get report "expectation")
                   :expected (json:get report "expected")
                   :received (json:get report "received")
                   :closing-behavior (json:get report "resultClose")
                   :closing-status (json:get report "behaviorClose"))))

(define-condition test-case-failure (error)
  ((test-case
    :initarg :test-case
    :initform nil))
  (:report (lambda (condition stream)
             (with-slots (test-case) condition
               (when test-case
                 (format stream "Description: ~A~%" (test-case-description test-case))
                 (format stream "Expectation: ~A~%" (test-case-expectation test-case))
                 (format stream "Expected: ~A~%" (json:encode (test-case-expected test-case)))
                 (format stream "Received: ~A~%" (json:encode (test-case-received test-case)))
                 (format stream "Closing Behavior: ~A~%" (test-case-closing-behavior test-case))
                 (format stream "Closing Status: ~A~%" (test-case-closing-status test-case)))))))

(defun run-wstest ()
  (unless (probe-file *wstest-executable-path*)
    (error "Missing wstest execuable (~A)" *wstest-executable-path*))
  (let* ((port *wstest-port*)
         (url (format nil "ws://127.0.0.1:~A" port))
         (outdir (namestring (uiop:default-temporary-directory)))
         (cases (sort (set-difference *wstest-cases* *wstest-complete*) 'string<))
         (spec-path (make-test-spec
                     :url url
                     :outdir outdir
                     :cases cases)))
    (when cases
      (let ((command (format nil "~A --mode fuzzingclient --spec ~A"
                             *wstest-executable-path*
                             spec-path)))
        (with-test-server port
          (uiop:run-program command
                            :output *standard-output*
                            :error-output *standard-output*))
        (delete-file spec-path)
        (appendf *wstest-complete* cases)
        (merge-pathnames "index.html" (uiop:default-temporary-directory))))))

(defmacro test-case (id)
  (let ((test-function-name (intern (format nil "TEST-CASE-~A" (id-string id #\-))))
        (test-name (intern (format nil "CASE-~A" (id-string id #\-)))))
    `(progn
       (defun ,test-function-name (&key fresh)
         (when fresh
           (setf *wstest-complete* (remove ,id *wstest-complete* :test 'equal)))
         (unless (find ,id *wstest-complete* :test 'equal)
           (run-wstest))
         (let ((report-pathname (merge-pathnames
                                 (format nil "wt_websocket_case_~A.json" (id-string ,id #\_))
                                 (uiop:default-temporary-directory))))
           (unless (probe-file report-pathname)
             (error "Missing test case report"))
           (let ((test-case (read-test-case-report report-pathname)))
             (when (eq (test-case-status test-case) :fail)
               (error 'test-case-failure
                      :test-case test-case)))))
       (test ,test-name
         (finishes (,test-function-name))))))

(defmacro test-group (title &body body)
  (declare (ignore title))
  (serapeum:walk-tree
   (lambda (form)
     (when (and (listp form) (eq (first form) 'test-case))
       (let ((test-case-id (second form)))
         (unless (find test-case-id *wstest-cases* :test 'equal)
           (setf *wstest-complete* nil)
           (push test-case-id *wstest-cases*)))))
   body)
  (setf *wstest-cases* (sort *wstest-cases* 'string<))
  `(progn ,@body))

(in-suite :websocket-test)

(test-group "1 Framing"
  (test-group "1.1 Text Messages"
    (test-case "1.1.1")
    (test-case "1.1.2")
    (test-case "1.1.3")
    (test-case "1.1.4")
    (test-case "1.1.5")
    (test-case "1.1.6")
    (test-case "1.1.7")
    (test-case "1.1.8"))
  (test-group "1.2 Binary Messages"
    (test-case "1.2.1")
    (test-case "1.2.2")
    (test-case "1.2.3")
    (test-case "1.2.4")
    (test-case "1.2.5")
    (test-case "1.2.6")
    (test-case "1.2.7")
    (test-case "1.2.8")))

(test-group "2 Pings/Pongs"
  (test-case "2.1")
  (test-case "2.2")
  (test-case "2.3")
  (test-case "2.4")
  (test-case "2.5")
  (test-case "2.6")
  (test-case "2.7")
  (test-case "2.8")
  (test-case "2.9")
  (test-case "2.10")
  (test-case "2.11"))

(test-group "3 Reserved Bits"
  (test-case "3.1")
  (test-case "3.2")
  (test-case "3.3")
  (test-case "3.4")
  (test-case "3.5")
  (test-case "3.6")
  (test-case "3.7"))

(test-group "4 Opcodes"
  (test-group "4.1 Non-control Opcodes"
    (test-case "4.1.1")
    (test-case "4.1.2")
    (test-case "4.1.3")
    (test-case "4.1.4")
    (test-case "4.1.5"))
  (test-group "4.2 Control Opcodes"
    (test-case "4.2.1")
    (test-case "4.2.2")
    (test-case "4.2.3")
    (test-case "4.2.4")
    (test-case "4.2.5")))

(test-group "5 Fragmentation"
  (test-case "5.1")
  (test-case "5.2")
  (test-case "5.3")
  (test-case "5.4")
  (test-case "5.5")
  (test-case "5.6")
  (test-case "5.7")
  (test-case "5.8")
  (test-case "5.9")
  (test-case "5.10")
  (test-case "5.11")
  (test-case "5.12")
  (test-case "5.13")
  (test-case "5.14")
  (test-case "5.15")
  (test-case "5.16")
  (test-case "5.17")
  (test-case "5.18")
  (test-case "5.19")
  (test-case "5.20"))

(test-group "6 UTF-8 Handling"
  (test-group "6.1 Valid UTF-8 with zero payload fragments"
    (test-case "6.1.1")
    (test-case "6.1.2")
    (test-case "6.1.3"))
  (test-group "6.2 Valid UTF-8 unfragmented, fragmented on code-points and within code-points"
    (test-case "6.2.1")
    (test-case "6.2.2")
    (test-case "6.2.3")
    (test-case "6.2.4"))
  (test-group "6.3 Invalid UTF-8 differently fragmented"
    (test-case "6.3.1")
    (test-case "6.3.2"))
  (test-group "6.4 Fail-fast on invalid UTF-8"
    (test-case "6.4.1")
    (test-case "6.4.2")
    (test-case "6.4.3")
    (test-case "6.4.4"))
  (test-group "6.5 Some valid UTF-8 sequences"
    (test-case "6.5.1")
    (test-case "6.5.2")
    (test-case "6.5.3")
    (test-case "6.5.4")
    (test-case "6.5.5"))
  (test-group "6.6 All prefixes of a valid UTF-8 string that contains multi-byte code points"
    (test-case "6.6.1")
    (test-case "6.6.2")
    (test-case "6.6.3")
    (test-case "6.6.4")
    (test-case "6.6.5")
    (test-case "6.6.6")
    (test-case "6.6.7")
    (test-case "6.6.8")
    (test-case "6.6.9")
    (test-case "6.6.10")
    (test-case "6.6.11"))
  (test-group "6.7 First possible sequence of a certain length"
    (test-case "6.7.1")
    (test-case "6.7.2")
    (test-case "6.7.3")
    (test-case "6.7.4"))
  (test-group "6.8 First possible sequence length 5/6 (invalid codepoints)"
    (test-case "6.8.1")
    (test-case "6.8.2"))
  (test-group "6.9 Last possible sequence of a certain length"
    (test-case "6.9.1")
    (test-case "6.9.2")
    (test-case "6.9.3")
    (test-case "6.9.4"))
  (test-group "6.10 Last possible sequence length 4/5/6 (invalid codepoints)"
    (test-case "6.10.1")
    (test-case "6.10.2")
    (test-case "6.10.3"))
  (test-group "6.11 Other boundary conditions"
    (test-case "6.11.1")
    (test-case "6.11.2")
    (test-case "6.11.3")
    (test-case "6.11.4")
    (test-case "6.11.5"))
  (test-group "6.12 Unexpected continuation bytes"
    (test-case "6.12.1")
    (test-case "6.12.2")
    (test-case "6.12.3")
    (test-case "6.12.4")
    (test-case "6.12.5")
    (test-case "6.12.6")
    (test-case "6.12.7")
    (test-case "6.12.8"))
  (test-group "6.13 Lonely start characters"
    (test-case "6.13.1")
    (test-case "6.13.2")
    (test-case "6.13.3")
    (test-case "6.13.4")
    (test-case "6.13.5"))
  (test-group "6.14 Sequences with last continuation byte missing"
    (test-case "6.14.1")
    (test-case "6.14.2")
    (test-case "6.14.3")
    (test-case "6.14.4")
    (test-case "6.14.5")
    (test-case "6.14.6")
    (test-case "6.14.7")
    (test-case "6.14.8")
    (test-case "6.14.9")
    (test-case "6.14.10"))
  (test-group "6.15 Concatenation of incomplete sequences"
    (test-case "6.15.1"))
  (test-group "6.16 Impossible bytes"
    (test-case "6.16.1")
    (test-case "6.16.2")
    (test-case "6.16.3"))
  (test-group "6.17 Examples of an overlong ASCII character"
    (test-case "6.17.1")
    (test-case "6.17.2")
    (test-case "6.17.3")
    (test-case "6.17.4")
    (test-case "6.17.5"))
  (test-group "6.18 Maximum overlong sequences"
    (test-case "6.18.1")
    (test-case "6.18.2")
    (test-case "6.18.3")
    (test-case "6.18.4")
    (test-case "6.18.5"))
  (test-group "6.19 Overlong representation of the NUL character"
    (test-case "6.19.1")
    (test-case "6.19.2")
    (test-case "6.19.3")
    (test-case "6.19.4")
    (test-case "6.19.5"))
  (test-group "6.20 Single UTF-16 surrogates"
    (test-case "6.20.1")
    (test-case "6.20.2")
    (test-case "6.20.3")
    (test-case "6.20.4")
    (test-case "6.20.5")
    (test-case "6.20.6")
    (test-case "6.20.7"))
  (test-group "6.21 Paired UTF-16 surrogates"
    (test-case "6.21.1")
    (test-case "6.21.2")
    (test-case "6.21.3")
    (test-case "6.21.4")
    (test-case "6.21.5")
    (test-case "6.21.6")
    (test-case "6.21.7")
    (test-case "6.21.8"))
  (test-group "6.22 Non-character code points (valid UTF-8)"
    (test-case "6.22.1")
    (test-case "6.22.2")
    (test-case "6.22.3")
    (test-case "6.22.4")
    (test-case "6.22.5")
    (test-case "6.22.6")
    (test-case "6.22.7")
    (test-case "6.22.8")
    (test-case "6.22.9")
    (test-case "6.22.10")
    (test-case "6.22.11")
    (test-case "6.22.12")
    (test-case "6.22.13")
    (test-case "6.22.14")
    (test-case "6.22.15")
    (test-case "6.22.16")
    (test-case "6.22.17")
    (test-case "6.22.18")
    (test-case "6.22.19")
    (test-case "6.22.20")
    (test-case "6.22.21")
    (test-case "6.22.22")
    (test-case "6.22.23")
    (test-case "6.22.24")
    (test-case "6.22.25")
    (test-case "6.22.26")
    (test-case "6.22.27")
    (test-case "6.22.28")
    (test-case "6.22.29")
    (test-case "6.22.30")
    (test-case "6.22.31")
    (test-case "6.22.32")
    (test-case "6.22.33")
    (test-case "6.22.34"))
  (test-group "6.23 Unicode specials (i.e. replacement char)"
    (test-case "6.23.1")
    (test-case "6.23.2")
    (test-case "6.23.3")
    (test-case "6.23.4")
    (test-case "6.23.5")
    (test-case "6.23.6")
    (test-case "6.23.7")))

(test-group "7 Close Handling"
  (test-group "7.1 Basic close behavior (fuzzer initiated)"
    (test-case "7.1.1")
    (test-case "7.1.2")
    (test-case "7.1.3")
    (test-case "7.1.4")
    (test-case "7.1.5")
    (test-case "7.1.6"))
  (test-group "7.3 Close frame structure: payload length (fuzzer initiated)"
    (test-case "7.3.1")
    (test-case "7.3.2")
    (test-case "7.3.3")
    (test-case "7.3.4")
    (test-case "7.3.5")
    (test-case "7.3.6"))
  (test-group "7.5 Close frame structure: payload value (fuzzer initiated)"
    (test-case "7.5.1"))
  (test-group "7.7 Close frame structure: valid close codes (fuzzer initiated)"
    (test-case "7.7.1")
    (test-case "7.7.2")
    (test-case "7.7.3")
    (test-case "7.7.4")
    (test-case "7.7.5")
    (test-case "7.7.6")
    (test-case "7.7.7")
    (test-case "7.7.8")
    (test-case "7.7.9")
    (test-case "7.7.10")
    (test-case "7.7.11")
    (test-case "7.7.12")
    (test-case "7.7.13"))
  (test-group "7.9 Close frame structure: invalid close codes (fuzzer initiated)"
    (test-case "7.9.1")
    (test-case "7.9.2")
    (test-case "7.9.3")
    (test-case "7.9.4")
    (test-case "7.9.5")
    (test-case "7.9.6")
    (test-case "7.9.7")
    (test-case "7.9.8")
    (test-case "7.9.9"))
  (test-group "7.13 Informational close information (fuzzer initiated)"
    (test-case "7.13.1")
    (test-case "7.13.2")))

;; (test-group "9 Limits/Performance"
;;   (test-group "9.1 Text Message (increasing size)"
;;     (test-case "9.1.1")
;;     (test-case "9.1.2")
;;     (test-case "9.1.3")
;;     (test-case "9.1.4")
;;     (test-case "9.1.5")
;;     (test-case "9.1.6"))
;;   (test-group "9.2 Binary Message (increasing size)"
;;     (test-case "9.2.1")
;;     (test-case "9.2.2")
;;     (test-case "9.2.3")
;;     (test-case "9.2.4")
;;     (test-case "9.2.5")
;;     (test-case "9.2.6"))
;;   (test-group "9.3 Fragmented Text Message (fixed size, increasing fragment size)"
;;     (test-case "9.3.1")
;;     (test-case "9.3.2")
;;     (test-case "9.3.3")
;;     (test-case "9.3.4")
;;     (test-case "9.3.5")
;;     (test-case "9.3.6")
;;     (test-case "9.3.7")
;;     (test-case "9.3.8")
;;     (test-case "9.3.9"))
;;   (test-group "9.4 Fragmented Binary Message (fixed size, increasing fragment size)"
;;     (test-case "9.4.1")
;;     (test-case "9.4.2")
;;     (test-case "9.4.3")
;;     (test-case "9.4.4")
;;     (test-case "9.4.5")
;;     (test-case "9.4.6")
;;     (test-case "9.4.7")
;;     (test-case "9.4.8")
;;     (test-case "9.4.9"))
;;   (test-group "9.5 Text Message (fixed size, increasing chop size)"
;;     (test-case "9.5.1")
;;     (test-case "9.5.2")
;;     (test-case "9.5.3")
;;     (test-case "9.5.4")
;;     (test-case "9.5.5")
;;     (test-case "9.5.6"))
;;   (test-group "9.6 Binary Text Message (fixed size, increasing chop size)"
;;     (test-case "9.6.1")
;;     (test-case "9.6.2")
;;     (test-case "9.6.3")
;;     (test-case "9.6.4")
;;     (test-case "9.6.5")
;;     (test-case "9.6.6"))
;;   (test-group "9.7 Text Message Roundtrip Time (fixed number, increasing size)"
;;     (test-case "9.7.1")
;;     (test-case "9.7.2")
;;     (test-case "9.7.3")
;;     (test-case "9.7.4")
;;     (test-case "9.7.5")
;;     (test-case "9.7.6"))
;;   (test-group "9.8 Binary Message Roundtrip Time (fixed number, increasing size)"
;;     (test-case "9.8.1")
;;     (test-case "9.8.2")
;;     (test-case "9.8.3")
;;     (test-case "9.8.4")
;;     (test-case "9.8.5")
;;     (test-case "9.8.6")))

;; (test-group "10 Misc"
;;   (test-group "10.1 Auto-Fragmentation"
;;     (test-case "10.1.1")))

;; (test-group "12 WebSocket Compression (different payloads)"
;;   (test-group "12.1 Large JSON data file (utf8, 194056 bytes)"
;;     (test-case "12.1.1")
;;     (test-case "12.1.2")
;;     (test-case "12.1.3")
;;     (test-case "12.1.4")
;;     (test-case "12.1.5")
;;     (test-case "12.1.6")
;;     (test-case "12.1.7")
;;     (test-case "12.1.8")
;;     (test-case "12.1.9")
;;     (test-case "12.1.10")
;;     (test-case "12.1.11")
;;     (test-case "12.1.12")
;;     (test-case "12.1.13")
;;     (test-case "12.1.14")
;;     (test-case "12.1.15")
;;     (test-case "12.1.16")
;;     (test-case "12.1.17")
;;     (test-case "12.1.18"))
;;   (test-group "12.2 Lena Picture, Bitmap 512x512 bw (binary, 263222 bytes)"
;;     (test-case "12.2.1")
;;     (test-case "12.2.2")
;;     (test-case "12.2.3")
;;     (test-case "12.2.4")
;;     (test-case "12.2.5")
;;     (test-case "12.2.6")
;;     (test-case "12.2.7")
;;     (test-case "12.2.8")
;;     (test-case "12.2.9")
;;     (test-case "12.2.10")
;;     (test-case "12.2.11")
;;     (test-case "12.2.12")
;;     (test-case "12.2.13")
;;     (test-case "12.2.14")
;;     (test-case "12.2.15")
;;     (test-case "12.2.16")
;;     (test-case "12.2.17")
;;     (test-case "12.2.18"))
;;   (test-group "12.3 Human readable text, Goethe's Faust I (German) (binary, 222218 bytes)"
;;     (test-case "12.3.1")
;;     (test-case "12.3.2")
;;     (test-case "12.3.3")
;;     (test-case "12.3.4")
;;     (test-case "12.3.5")
;;     (test-case "12.3.6")
;;     (test-case "12.3.7")
;;     (test-case "12.3.8")
;;     (test-case "12.3.9")
;;     (test-case "12.3.10")
;;     (test-case "12.3.11")
;;     (test-case "12.3.12")
;;     (test-case "12.3.13")
;;     (test-case "12.3.14")
;;     (test-case "12.3.15")
;;     (test-case "12.3.16")
;;     (test-case "12.3.17")
;;     (test-case "12.3.18"))
;;   (test-group "12.4 Large HTML file (utf8, 263527 bytes)"
;;     (test-case "12.4.1")
;;     (test-case "12.4.2")
;;     (test-case "12.4.3")
;;     (test-case "12.4.4")
;;     (test-case "12.4.5")
;;     (test-case "12.4.6")
;;     (test-case "12.4.7")
;;     (test-case "12.4.8")
;;     (test-case "12.4.9")
;;     (test-case "12.4.10")
;;     (test-case "12.4.11")
;;     (test-case "12.4.12")
;;     (test-case "12.4.13")
;;     (test-case "12.4.14")
;;     (test-case "12.4.15")
;;     (test-case "12.4.16")
;;     (test-case "12.4.17")
;;     (test-case "12.4.18"))
;;   (test-group "12.5 A larger PDF (binary, 1042328 bytes)"
;;     (test-case "12.5.1")
;;     (test-case "12.5.2")
;;     (test-case "12.5.3")
;;     (test-case "12.5.4")
;;     (test-case "12.5.5")
;;     (test-case "12.5.6")
;;     (test-case "12.5.7")
;;     (test-case "12.5.8")
;;     (test-case "12.5.9")
;;     (test-case "12.5.10")
;;     (test-case "12.5.11")
;;     (test-case "12.5.12")
;;     (test-case "12.5.13")
;;     (test-case "12.5.14")
;;     (test-case "12.5.15")
;;     (test-case "12.5.16")
;;     (test-case "12.5.17")
;;     (test-case "12.5.18")))

;; (test-group "13 WebSocket Compression (different parameters)"
;;   (test-group "13.1 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(False, 0)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(False, 0)]"
;;     (test-case "13.1.1")
;;     (test-case "13.1.2")
;;     (test-case "13.1.3")
;;     (test-case "13.1.4")
;;     (test-case "13.1.5")
;;     (test-case "13.1.6")
;;     (test-case "13.1.7")
;;     (test-case "13.1.8")
;;     (test-case "13.1.9")
;;     (test-case "13.1.10")
;;     (test-case "13.1.11")
;;     (test-case "13.1.12")
;;     (test-case "13.1.13")
;;     (test-case "13.1.14")
;;     (test-case "13.1.15")
;;     (test-case "13.1.16")
;;     (test-case "13.1.17")
;;     (test-case "13.1.18"))
;;   (test-group "13.2 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(True, 0)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(True, 0)]"
;;     (test-case "13.2.1")
;;     (test-case "13.2.2")
;;     (test-case "13.2.3")
;;     (test-case "13.2.4")
;;     (test-case "13.2.5")
;;     (test-case "13.2.6")
;;     (test-case "13.2.7")
;;     (test-case "13.2.8")
;;     (test-case "13.2.9")
;;     (test-case "13.2.10")
;;     (test-case "13.2.11")
;;     (test-case "13.2.12")
;;     (test-case "13.2.13")
;;     (test-case "13.2.14")
;;     (test-case "13.2.15")
;;     (test-case "13.2.16")
;;     (test-case "13.2.17")
;;     (test-case "13.2.18"))
;;   (test-group "13.3 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(False, 8)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(False, 8)]"
;;     (test-case "13.3.1")
;;     (test-case "13.3.2")
;;     (test-case "13.3.3")
;;     (test-case "13.3.4")
;;     (test-case "13.3.5")
;;     (test-case "13.3.6")
;;     (test-case "13.3.7")
;;     (test-case "13.3.8")
;;     (test-case "13.3.9")
;;     (test-case "13.3.10")
;;     (test-case "13.3.11")
;;     (test-case "13.3.12")
;;     (test-case "13.3.13")
;;     (test-case "13.3.14")
;;     (test-case "13.3.15")
;;     (test-case "13.3.16")
;;     (test-case "13.3.17")
;;     (test-case "13.3.18"))
;;   (test-group "13.4 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(False, 15)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(False, 15)]"
;;     (test-case "13.4.1")
;;     (test-case "13.4.2")
;;     (test-case "13.4.3")
;;     (test-case "13.4.4")
;;     (test-case "13.4.5")
;;     (test-case "13.4.6")
;;     (test-case "13.4.7")
;;     (test-case "13.4.8")
;;     (test-case "13.4.9")
;;     (test-case "13.4.10")
;;     (test-case "13.4.11")
;;     (test-case "13.4.12")
;;     (test-case "13.4.13")
;;     (test-case "13.4.14")
;;     (test-case "13.4.15")
;;     (test-case "13.4.16")
;;     (test-case "13.4.17")
;;     (test-case "13.4.18"))
;;   (test-group "13.5 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(True, 8)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(True, 8)]"
;;     (test-case "13.5.1")
;;     (test-case "13.5.2")
;;     (test-case "13.5.3")
;;     (test-case "13.5.4")
;;     (test-case "13.5.5")
;;     (test-case "13.5.6")
;;     (test-case "13.5.7")
;;     (test-case "13.5.8")
;;     (test-case "13.5.9")
;;     (test-case "13.5.10")
;;     (test-case "13.5.11")
;;     (test-case "13.5.12")
;;     (test-case "13.5.13")
;;     (test-case "13.5.14")
;;     (test-case "13.5.15")
;;     (test-case "13.5.16")
;;     (test-case "13.5.17")
;;     (test-case "13.5.18"))
;;   (test-group "13.6 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(True, 15)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(True, 15)]"
;;     (test-case "13.6.1")
;;     (test-case "13.6.2")
;;     (test-case "13.6.3")
;;     (test-case "13.6.4")
;;     (test-case "13.6.5")
;;     (test-case "13.6.6")
;;     (test-case "13.6.7")
;;     (test-case "13.6.8")
;;     (test-case "13.6.9")
;;     (test-case "13.6.10")
;;     (test-case "13.6.11")
;;     (test-case "13.6.12")
;;     (test-case "13.6.13")
;;     (test-case "13.6.14")
;;     (test-case "13.6.15")
;;     (test-case "13.6.16")
;;     (test-case "13.6.17")
;;     (test-case "13.6.18"))
;;   (test-group "13.7 Large JSON data file (utf8, 194056 bytes) - client offers (requestNoContextTakeover, requestMaxWindowBits): [(True, 8), (True, 0), (False, 0)] / server accept (requestNoContextTakeover, requestMaxWindowBits): [(True, 8), (True, 0), (False, 0)]"
;;     (test-case "13.7.1")
;;     (test-case "13.7.2")
;;     (test-case "13.7.3")
;;     (test-case "13.7.4")
;;     (test-case "13.7.5")
;;     (test-case "13.7.6")
;;     (test-case "13.7.7")
;;     (test-case "13.7.8")
;;     (test-case "13.7.9")
;;     (test-case "13.7.10")
;;     (test-case "13.7.11")
;;     (test-case "13.7.12")
;;     (test-case "13.7.13")
;;     (test-case "13.7.14")
;;     (test-case "13.7.15")
;;     (test-case "13.7.16")
;;     (test-case "13.7.17")
;;     (test-case "13.7.18")))
