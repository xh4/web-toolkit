(in-package :http)

(defun read-until-next-boundary (stream boundary &optional discard output-stream)
  ;; Read until [CRLF]--boundary[--][transport-padding]CRLF
  ;; States:     1 2  345        67  8                 9 10
  (when (stringp boundary)
    (setf boundary (babel:string-to-octets boundary)))
  ;; (let ((length (length boundary)))
  ;;   (unless (<= 1 length 70)
  ;;     (warn "Boundary has invalid length -- must be between 1 and 70, but is: ~S" length))
  ;;   (when (lwsp-char-p (schar boundary (1- length)))
  ;;     (warn "Boundary has trailing whitespace: ~S" boundary)))
  (flet ((run (result)
           "This one writes everything up to a boundary to RESULT stream,
            and returns false if the closing delimiter has been read, and
            true otherwise."
           (let ((state 1)
                 (boundary-index 0)
                 (boundary-length (length boundary))
                 (closed nil)
                 (queued-bytes (make-array 4 :initial-element nil))
                 (queue-index 0)
                 byte
                 (leave-byte nil))

             (flet ((write-queued-bytes ()
                      (dotimes (i queue-index)
                        (write-byte (aref queued-bytes i) result))
                      (setf queue-index 0))

                    (enqueue-byte ()
                      (setf (aref queued-bytes queue-index) byte)
                      (incf queue-index)))

               (loop

                  (if leave-byte
                      (setq leave-byte nil)
                      (setq byte (read-byte stream)))

                  #-(and)
                  (format t "~&S:~D QI:~D BI:~2,'0D CH:~:[~;*~]~S~%"
                          state queue-index boundary-index leave-char char)

                  (case state
                    (1 ;; optional starting CR
                     (cond ((= byte #.(char-code #\Return))
                            (enqueue-byte)
                            (setq state 2))
                           ((= byte #.(char-code #\-))
                            (setq leave-byte t
                                  state 3))
                           (t
                            (write-byte byte result))))

                    (2 ;; optional starting LF
                     (cond ((= byte #.(char-code #\Linefeed))
                            (enqueue-byte)
                            (setq state 3))
                           (t
                            (write-queued-bytes)
                            (setq leave-byte t
                                  state 1))))

                    (3 ;; first dash in dash-boundary
                     (cond ((= byte #.(char-code #\-))
                            (enqueue-byte)
                            (setq state 4))
                           (t
                            (write-queued-bytes)
                            (setq leave-byte t
                                  state 1))))

                    (4 ;; second dash in dash-boundary
                     (cond ((= byte #.(char-code #\-))
                            (enqueue-byte)
                            (setq state 5))
                           (t
                            (write-queued-bytes)
                            (setq leave-byte t
                                  state 1))))

                    (5 ;; boundary
                     (cond ((= byte (aref boundary boundary-index))
                            (incf boundary-index)
                            (when (= boundary-index boundary-length)
                              (setq state 6)))
                           (t
                            (write-queued-bytes)
                            (write-sequence boundary result :end boundary-index)
                            (setq boundary-index 0
                                  leave-byte t
                                  state 1))))

                    (6 ;; first dash in close-delimiter
                     (cond ((= byte #.(char-code #\-))
                            (setq state 7))
                           (t
                            (setq leave-byte t)
                            (setq state 8))))

                    (7 ;; second dash in close-delimiter
                     (cond ((= byte #.(char-code #\-))
                            (setq closed t
                                  state 8))
                           (t
                            ;; this is a strange situation -- only two dashes, linear
                            ;; whitespace or CR is allowed after boundary, but there was
                            ;; a single dash...  One thing is clear -- this is not a
                            ;; close-delimiter.  Hence this is garbage what we're looking
                            ;; at!
                            (warn "Garbage where expecting close-delimiter!")
                            (setq leave-byte t)
                            (setq state 8))))

                    (8 ;; transport-padding (LWSP* == [#\space #\tab]*)
                     (cond ((lwsp-byte-p byte)
                            ;; ignore these
                            )
                           (t
                            (setq leave-byte t)
                            (setq state 9))))

                    (9 ;; CR
                     (cond ((= byte #.(char-code #\Return))
                            (setq state 10))
                           (t
                            (warn "Garbage where expecting CR!"))))

                    (10 ;; LF
                     (cond ((= byte #.(char-code #\Linefeed))
                            ;; the end
                            (return))
                           (t
                            (warn "Garbage where expecting LF!")))))))
             (not closed))))
    (if discard
        (let ((stream (make-broadcast-stream)))
          (values nil (run stream)))
        (let* ((stream (or output-stream (babel-streams:make-in-memory-output-stream)))
               (closed (run stream)))
          (values (or output-stream (babel-streams:get-output-stream-sequence stream))
                  closed)))))

(defun lwsp-byte-p (byte)
  (or (= byte #.(char-code #\Space))
      (= byte #.(char-code #\Tab))))

(defun read-part-header (stream &key (parse t))
  (loop for line = (read-line stream)
     while (and line (> (length line) 0))
     collect line into header
     finally (return (if parse (parse-part-header header) header))))

(defun parse-part-header (header)
  (let (content-type ~name filename)
    (loop for line in header
       do (let ((list (cl-ppcre:split ":" line :limit 2)))
            (if (= 2 (length list))
                (let ((name (first list))
                      (value (second list)))
                  (setf value (trim-whitespace value))
                  (cond
                    ((string-equal "Content-Disposition" name)
                     (let ((segments (cl-ppcre:split ";\\s*" value)))
                       (loop for segment in segments
                          for (name value) = (cl-ppcre:split "=" segment)
                          do (cond
                               ((equal "name" name) (setf ~name (subseq value 1 (1- (length value)))))
                               ((equal "filename" name) (setf filename (subseq value 1 (1- (length value)))))))))
                    ((string-equal "Content-Type" name)
                     (setf content-type value))))
                (error "Malformed part header"))))
    (list :content-type content-type
          :name ~name
          :filename filename)))

(defun read-multipart-form-data (stream boundary)
  (unless (nth-value 1 (read-until-next-boundary stream boundary t))
    (return-from read-multipart-form-data nil))
  (let ((parts '()))
    (loop
       (let* ((header (read-part-header stream))
              (name (getf header :name)))
         (multiple-value-bind (data has-next)
             (read-until-next-boundary stream boundary)
           (push (cons name data) parts)
           (unless has-next
             (return)))))
    (nreverse parts)))

(defvar +ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun random-string (&optional (length 32) (alphabet +ascii-alphabet+))
  (loop with id = (make-string length)
     with alphabet-length = (length alphabet)
     for i below length
     do (setf (cl:aref id i)
              (cl:aref alphabet (random alphabet-length)))
     finally (return id)))

(defun write-multipart-form-data (stream form)
  (let ((boundary (concatenate 'string "WT-HTTP-" (random-string))))
    (loop for field in (form-fields form)
       for index from 0
       for name = (form-field-name field)
       for value = (form-field-value field)
       do (when (> index 0)
            (write-sequence
             (babel:string-to-octets (format nil "~C~C" #\Return #\Newline))
             stream))
         (write-sequence
          (babel:string-to-octets (format nil "--~A~C~C" boundary #\Return #\Newline))
          stream)
         (write-sequence
          (babel:string-to-octets (format nil "Content-Disposition: form-data; name=\"~A\"~C~C"
                                          name #\Return #\Newline))
          stream)
         (write-sequence
          (babel:string-to-octets (format nil "~C~C" #\Return #\Newline))
          stream)
         (write-sequence
          (babel:string-to-octets value)
          stream)
         (write-sequence
          (babel:string-to-octets (format nil "~C~C--~A" #\Return #\Newline boundary))
          stream)
       finally
         (when (and (> (length (form-fields form)) 0)
                    (= index (1- (length (form-fields form)))))
           (write-sequence
            (babel:string-to-octets (format nil "--~C~C" #\Return #\Newline))
            stream)))
    boundary))
