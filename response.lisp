(in-package :wt.http)

(defgeneric response-type (response)
  (:method ((response response))
    (case (wt.proto.http:type response)
      (0 :reply)
      (1 :stream)
      (2 :sendfile))))

(defgeneric (setf response-type) (value response)
  (:method (value (response response))
    (check-type value keyword)
    (setf (wt.proto.http:type response)
          (case value
            (:reply 0)
            (:stream 1)
            (:sendfile 2)))))

(defgeneric response-code (response)
  (:method ((response response))
    (wt.proto.http:code response)))

(defgeneric (setf response-code) (value response)
  (:method (value (response response))
    (check-type value integer)
    (setf (wt.proto.http:code response) value)))

(defgeneric response-headers (response)
  (:method ((response response))
    (loop for header across (wt.proto.http:headers response)
       collect
         (cons (pb:string-value (wt.proto.http:key header))
               (pb:string-value (wt.proto.http:value header))))))

(defgeneric (setf response-headers) (value response)
  (:method (value (response response))
    (check-type value list)
    (let ((headers (loop for item in value
                      for name = (car item)
                      for value = (cdr item)
                      for header = (make-instance 'response-headers-entry)
                      do
                        (progn
                          (check-type name string)
                          (check-type value string)
                          (setf (wt.proto.http:key header) (pb:string-field name))
                          (setf (wt.proto.http:value header) (pb:string-field value)))
                      collect header)))
      (setf (wt.proto.http:headers response)
            (make-array
             (length headers)
             :element-type 'response-headers-entry
             :initial-contents headers)))))

(defgeneric response-body (response)
  (:method ((response response))
    (wt.proto.http:body response)))

(defgeneric (setf response-body) (value response)
  (:method ((value string) (response response))
    (setf (response-body response) (flex:string-to-octets value :external-format :utf-8)))
  (:method (value (response response))
    (check-type value (simple-array (unsigned-byte 8)))
    (setf (wt.proto.http:body response) value)))

(defgeneric response-path (response)
  (:method ((response response))
    (pb:string-value (wt.proto.http:path response))))

(defgeneric (setf response-path) (value response)
  (:method ((value string) (response response))
    (setf (wt.proto.http:path response) (pb:string-field value)))
  (:method ((value pathname) (response response))
    (setf (response-path response) (namestring value))))
