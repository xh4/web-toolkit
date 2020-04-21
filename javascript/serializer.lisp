(in-package :javascript)

(defmacro define-serialize-method (object (stream) &body body)
  (unless (listp object) (setf object `(,object ,object)))
  `(defmethod serialize (,object &optional ,stream)
     (let ((string-stream-p (null ,stream)))
       (when string-stream-p (setf ,stream (make-string-output-stream)))
       ,@body
       (when string-stream-p
         (get-output-stream-string ,stream)))))

(defgeneric serialize (object &optional stream))

(define-serialize-method identifier (stream)
  (format stream "~A" (identifier-name identifier)))

(define-serialize-method keyword (stream)
  (format stream "~A" (keyword-name keyword)))

(define-serialize-method boolean-literal (stream))

(define-serialize-method null-literal (stream))

(define-serialize-method numeric-literal (stream))

(define-serialize-method string-literal (stream))

(define-serialize-method reg-exp-literal (stream))

(define-serialize-method template-literal (stream))

(define-serialize-method punctuator (stream))

(define-serialize-method module (stream))

(define-serialize-method script (stream))

(define-serialize-method function (stream))

(define-serialize-method block-statement (stream))

(define-serialize-method empty-statement (stream))

(define-serialize-method debugger-statement (stream))

(define-serialize-method with-statement (stream))

(define-serialize-method return-statement (stream))

;; check
(define-serialize-method labeled-statement (stream))

(define-serialize-method break-statement (stream))

(define-serialize-method continue-statement (stream))

(define-serialize-method if-statement (stream))

(define-serialize-method switch-statement (stream))

(define-serialize-method throw-statement (stream))

(define-serialize-method try-statement (stream))

(define-serialize-method catch-clause (stream))

(define-serialize-method while-statement (stream))

(define-serialize-method do-while-statement (stream))

(define-serialize-method for-statement (stream))

(define-serialize-method for-in-statement (stream))

(define-serialize-method for-of-statement (stream))

(define-serialize-method function-declaration (stream))

(define-serialize-method variable-declaration (stream))

(define-serialize-method super (stream))

(define-serialize-method this-expression (stream))

(define-serialize-method array-expression (stream))

(define-serialize-method object-expression (stream))

(define-serialize-method property (stream))

(define-serialize-method function-expression (stream))

(define-serialize-method unary-expression (stream))

(define-serialize-method update-expression (stream))

(define-serialize-method binary-expression (stream))

(define-serialize-method assignment-expression (stream))

(define-serialize-method logical-expression (stream))

(define-serialize-method member-expression (stream))

(define-serialize-method computed-member-expression (stream))

(define-serialize-method static-member-expression (stream))

(define-serialize-method conditional-expression (stream))

(define-serialize-method call-expression (stream))

(define-serialize-method new-expression (stream))

(define-serialize-method sequence-expression (stream))

(define-serialize-method spread-element (stream))

(define-serialize-method arrow-function-expression (stream))

(define-serialize-method await-expression (stream))

(define-serialize-method yield-expression (stream))

(define-serialize-method tagged-template-expression (stream))

(define-serialize-method template-element (stream))

(define-serialize-method assignment-property (stream))

(define-serialize-method object-pattern (stream))

(define-serialize-method array-pattern (stream))

(define-serialize-method rest-element (stream))

(define-serialize-method assignment-pattern (stream))

(define-serialize-method class (stream))

(define-serialize-method class-body (stream))

(define-serialize-method method-definition (stream))

(define-serialize-method class-declaration (stream))

(define-serialize-method class-expression (stream))

(define-serialize-method meta-property (stream))

(define-serialize-method module-declaration (stream))

(define-serialize-method module-specifier (stream))

(define-serialize-method import-declaration (stream))

(define-serialize-method import-specifier (stream))

(define-serialize-method import-default-specifier (stream))

(define-serialize-method import-namespace-specifier (stream))

(define-serialize-method export-named-declaration (stream))

(define-serialize-method export-specifier (stream))

(define-serialize-method anonymous-default-exported-function-declaration (stream))

(define-serialize-method anonymous-default-exported-class-declaration (stream))

(define-serialize-method export-default-declaration (stream))

(define-serialize-method export-all-declaration (stream))