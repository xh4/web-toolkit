(in-package :http)

(eval-when (:compile-toplevel :execute :load-toplevel)

  (defclass status ()
    ((keyword
      :initarg :keyword
      :reader status-keyword)
     (code
      :initarg :code
      :reader status-code)
     (reason-phrase
      :initarg :reason-phrase
      :reader status-reason-phrase)))

  (defvar *status-code-mapping-table* (make-hash-table))
  (defvar *status-keyword-mapping-table* (make-hash-table))

  (defmacro define-response-status (keyword code reason-phrase)
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (let ((status (make-instance 'status
                                    :keyword ,keyword
                                    :code ,code
                                    :reason-phrase ,reason-phrase)))
         (setf (gethash ,code *status-code-mapping-table*) status)
         (setf (gethash ,keyword *status-keyword-mapping-table*) status)
         status))))

(defmethod print-object ((status status) stream)
  (print-unreadable-object (status stream)
    (format stream "~A ~A"
            (status-code status)
            (status-reason-phrase status))))

;; https://tools.ietf.org/html/rfc7231#section-6.1

(define-response-status :continue                        100 "Continue")
(define-response-status :switching-protocols             101 "Switching Protocols")

(define-response-status :ok                              200 "OK")
(define-response-status :created                         201 "Created")
(define-response-status :accepted                        202 "Accepted")
(define-response-status :non-authoritative-information   203 "Non-Authoritative Information")
(define-response-status :no-content                      204 "No Content")
(define-response-status :reset-content                   205 "Reset Content")
(define-response-status :partial-content                 206 "Partial Content")

(define-response-status :multiple-choices                300 "Multiple Choices")
(define-response-status :moved-permanently               301 "Moved Permanently")
(define-response-status :found                           302 "Found")
(define-response-status :see-other                       303 "See Other")
(define-response-status :not-modified                    304 "Not Modified")
(define-response-status :temporary-redirect              307 "Temporary Redirect")
(define-response-status :permanent-redirect              308 "Permanent Redirect")

(define-response-status :bad-request                     400 "Bad Request")
(define-response-status :unauthorized                    401 "Unauthorized")
(define-response-status :forbidden                       403 "Forbidden")
(define-response-status :not-found                       404 "Not Found")
(define-response-status :method-not-allowed              405 "Method Not Allowed")
(define-response-status :not-acceptable                  406 "Not Acceptable")
(define-response-status :proxy-authentication-required   407 "Proxy Authentication Required")
(define-response-status :request-timeout                 408 "Request Timeout")
(define-response-status :conflict                        409 "Conflict")
(define-response-status :gone                            410 "Gone")
(define-response-status :length-required                 411 "Length Required")
(define-response-status :precondition-failed             412 "Precondition Failed")
(define-response-status :payload-too-large               413 "Payload Too Large")
(define-response-status :uri-too-large                   414 "URI Too Long")
(define-response-status :unsupported-media-type          415 "Unsupported Media Type")
(define-response-status :requested-range-not-satisfiable 416 "Requested Range Not Satisfiable")
(define-response-status :expectation-failed              417 "Expectation Failed")
(define-response-status :too-early                       425 "Too Early")
(define-response-status :upgrade-required                426 "Upgrade Required")
(define-response-status :precondition-required           428 "Precondition Required")
(define-response-status :too-many-requests               429 "Too Many Requests")
(define-response-status :request-header-fields-too-large 431 "Request Header Fields Too Large")
(define-response-status :unavailable-for-legal-reasons   451 "Unavailable For Legal Reasons")

(define-response-status :internal-server-error           500 "Internal Server Error")
(define-response-status :not-implemented                 501 "Not Implemented")
(define-response-status :bad-gateway                     502 "Bad Gateway")
(define-response-status :service-unavailable             503 "Service Unavailable")
(define-response-status :gateway-timeout                 504 "Gateway Timeout")
(define-response-status :http-version-not-supported      505 "HTTP Version Not Supported")
(define-response-status :network-authentication-required 511 "Network Authentication Required")
