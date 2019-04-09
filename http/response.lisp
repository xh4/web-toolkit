(in-package :wt.http)


(defgeneric response-status (response))

(defgeneric (setf response-status) (value response))

(defgeneric response-header (response))

(defgeneric (setf response-header) (value response))

(defgeneric response-body (response))

(defgeneric (setf response-body) (value response))


(eval-when (:compile-toplevel :execute :load-toplevel)

  (defvar *http-statuses* '())

  (defmacro define-http-response-status (keyword code reason-phrase)
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (when (not (find ,keyword *http-statuses* :key 'status-keyword))
         (setf *http-statuses*
               (nconc *http-statuses*
                      (list (make-instance 'http-status
                                           :keyword ,keyword
                                           :code ,code
                                           :reason-phrase ,reason-phrase))))))))

;; https://tools.ietf.org/html/rfc7231#section-6.1

(define-http-response-status :continue                        100 "Continue")
(define-http-response-status :switching-protocols             101 "Switching Protocols")

(define-http-response-status :ok                              200 "OK")
(define-http-response-status :created                         201 "Created")
(define-http-response-status :accepted                        202 "Accepted")
(define-http-response-status :non-authoritative-information   203 "Non-Authoritative Information")
(define-http-response-status :no-content                      204 "No Content")
(define-http-response-status :reset-content                   205 "Reset Content")
(define-http-response-status :partial-content                 206 "Partial Content")

(define-http-response-status :multiple-choices                300 "Multiple Choices")
(define-http-response-status :moved-permanently               301 "Moved Permanently")
(define-http-response-status :found                           302 "Found")
(define-http-response-status :see-other                       303 "See Other")
(define-http-response-status :not-modified                    304 "Not Modified")
(define-http-response-status :temporary-redirect              307 "Temporary Redirect")
(define-http-response-status :permanent-redirect              308 "Permanent Redirect")

(define-http-response-status :bad-request                     400 "Bad Request")
(define-http-response-status :unauthorized                    401 "Unauthorized")
(define-http-response-status :forbidden                       403 "Forbidden")
(define-http-response-status :not-found                       404 "Not Found")
(define-http-response-status :method-not-allowed              405 "Method Not Allowed")
(define-http-response-status :not-acceptable                  406 "Not Acceptable")
(define-http-response-status :proxy-authentication-required   407 "Proxy Authentication Required")
(define-http-response-status :request-timeout                 408 "Request Timeout")
(define-http-response-status :conflict                        409 "Conflict")
(define-http-response-status :gone                            410 "Gone")
(define-http-response-status :length-required                 411 "Length Required")
(define-http-response-status :precondition-failed             412 "Precondition Failed")
(define-http-response-status :payload-too-large               413 "Payload Too Large")
(define-http-response-status :uri-too-large                   414 "URI Too Long")
(define-http-response-status :unsupported-media-type          415 "Unsupported Media Type")
(define-http-response-status :requested-range-not-satisfiable 416 "Requested Range Not Satisfiable")
(define-http-response-status :expectation-failed              417 "Expectation Failed")
(define-http-response-status :too-early                       425 "Too Early")
(define-http-response-status :upgrade-required                426 "Upgrade Required")
(define-http-response-status :precondition-required           428 "Precondition Required")
(define-http-response-status :too-many-requests               429 "Too Many Requests")
(define-http-response-status :request-header-fields-too-large 431 "Request Header Fields Too Large")
(define-http-response-status :unavailable-for-legal-reasons   451 "Unavailable For Legal Reasons")

(define-http-response-status :internal-server-error           500 "Internal Server Error")
(define-http-response-status :not-implemented                 501 "Not Implemented")
(define-http-response-status :bad-gateway                     502 "Bad Gateway")
(define-http-response-status :service-unavailable             503 "Service Unavailable")
(define-http-response-status :gateway-timeout                 504 "Gateway Timeout")
(define-http-response-status :http-version-not-supported      505 "HTTP Version Not Supported")
(define-http-response-status :network-authentication-required 511 "Network Authentication Required")
