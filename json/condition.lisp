(in-package :json)

(define-condition json-syntax-error (simple-error stream-error)
  ((stream-file-position :reader stream-error-stream-file-position
                         :initarg :stream-file-position))
  (:report
   (lambda (condition stream)
     (format stream "~? [in ~S~@[ at position ~D~]]"
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)
             (stream-error-stream condition)
             (stream-error-stream-file-position condition))))
  (:documentation
   "Signalled when non-well-formed JSON data are encountered."))

(defun json-syntax-error (stream format-control &rest format-arguments)
  "Signal a JSON-SYNTAX-ERROR condition."
  (error 'json-syntax-error
         :stream stream
         :stream-file-position (file-position stream)
         :format-control format-control
         :format-arguments format-arguments))
