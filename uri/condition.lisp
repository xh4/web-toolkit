(in-package :uri)

(define-condition uri-condition () ())

(define-condition uri-error (uri-condition error) ())

(define-condition parse-uri-error (uri-error)
  ((uri-string
    :initarg :uri-string
    :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (uri-string) condition
       (format stream "Unable to parse URI string ~S" uri-string)))))

(define-condition merge-uri-error (uri-error)
  ((base-uri
     :initarg :base-uri
     :initform nil)
   (relative-uri
    :initarg :relative-uri
    :initform nil)
   (message
    :initarg :message
    :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (base-uri relative-uri message) condition
       (format stream "Unable to merge URI ~S into ~S" relative-uri base-uri)
       (when message (format stream "~%~A" message))))))

(define-condition uri-component-error (uri-error)
  ((component
    :initarg :component
    :initform nil)
   (value
    :initarg :value
    :initform nil)
   (message
    :initarg :message
    :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (component value message) condition
       (format stream "Unable to use the value ~S for URI component ~A"
               value component)
       (when message (format stream "~%~A" message))))))

(define-condition update-uri-error (uri-component-error)
  ((uri
    :initarg :uri
    :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (uri component value message) condition
       (format stream "Unable to update the ~A component of URI ~S using value ~A"
               component uri value)
       (when message (format stream "~%~A" message))))))

(define-condition construct-uri-error (uri-error)
  ((arguments
     :initarg :arguments
     :initform nil)
   (message
    :initarg :message
    :initform nil))
  (:report
   (lambda (condition stream)
     (with-slots (arguments message) condition
       (format stream "Unable to construct URI using arguments ~S" arguments)
       (when message (format stream "~%~A" message))))))
