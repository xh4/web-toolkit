(in-package :javascript)

(defmacro define-serialize-method (object (stream) &body body)
  (unless (listp object) (setf object `(,object ,object)))
  `(defmethod serialize (,object &optional ,stream)
     (let ((string-stream-p (null ,stream)))
       (when string-stream-p (setf ,stream (make-string-output-stream)))
       ,@(if body
             body
           `((error "Serialize method for ~A not implemented" ',(if (listp object)
                                                                    (second object)
                                                                  object))))
       (when string-stream-p
         (get-output-stream-string ,stream)))))

(defgeneric serialize (object &optional stream))

(define-serialize-method string (stream)
  (format stream "~A" string))

(define-serialize-method identifier (stream)
  (format stream "~A" (identifier-name identifier)))

(define-serialize-method keyword (stream)
  (format stream "~A" (keyword-name keyword)))

(define-serialize-method boolean-literal (stream)
  (format stream "~A" (literal-value boolean-literal)))

(define-serialize-method null-literal (stream)
  (format stream "null"))

(define-serialize-method numeric-literal (stream)
  (format stream "~A" (literal-value numeric-literal)))

(define-serialize-method string-literal (stream)
  (format stream "~S" (literal-value string-literal)))

(define-serialize-method reg-exp-literal (stream)
  (with-slots (pattern flags) reg-exp-literal
    (format stream "/~A/~A" pattern flags)))

(define-serialize-method template-literal (stream)
  (format stream "`~A`" (literal-value template-literal)))

(define-serialize-method punctuator (stream)
  (format stream "~A" (punctuator-value punctuator)))

(define-serialize-method module (stream)
  (call-next-method))

(define-serialize-method script (stream)
  (with-slots (body) script
    (loop for node in (ensure-list body)
          do (serialize node stream))))

(define-serialize-method function (stream))

(define-serialize-method expression-statement (stream)
  (with-slots (expression) expression-statement
    (serialize expression stream)
    (format stream ";")))

(define-serialize-method block-statement (stream)
  (with-slots (body) block-statement
    (format stream "{")
    (loop for node in (ensure-list body)
          do (serialize node stream))
    (format stream "}")))

(define-serialize-method empty-statement (stream)
  (format stream ""))

(define-serialize-method debugger-statement (stream))

(define-serialize-method with-statement (stream))

(define-serialize-method return-statement (stream)
  (with-slots (argument) return-statement
    (format stream "return")
    (when argument
      (format stream " ")
      (serialize argument stream))
    (format stream ";")))

(define-serialize-method labeled-statement (stream))

(define-serialize-method break-statement (stream)
  (format stream "break;"))

(define-serialize-method continue-statement (stream)
  (format stream "continue;"))

(define-serialize-method if-statement (stream)
  (with-slots (test consequent alternate) if-statement
    (format stream "if (")
    (serialize test stream)
    (format stream ")")
    (serialize consequent stream)
    (when alternate
      (format stream "else")
      (serialize alternate stream))))

(define-serialize-method switch-statement (stream)
  (with-slots (discriminant cases) switch-statement
    (format stream "switch ")
    (format stream "(")
    (serialize discriminant stream)
    (format stream ")")
    (format stream "{")
    (loop for case in cases
          do (serialize case stream))
    (format stream "}")))

(define-serialize-method switch-case (stream)
  (with-slots (test consequent) switch-case
    (if test
        (progn
          (format stream "case ")
          (serialize test stream)
          (format stream ":"))
      (format stream "default:"))
    (loop for node in consequent
          do (serialize node stream))))

(define-serialize-method throw-statement (stream)
  (with-slots (argument) throw-statement
    (format stream "throw ")
    (serialize argument stream)))

(define-serialize-method try-statement (stream)
  (with-slots (block handler finalizer) try-statement
    (format stream "try ")
    (serialize block stream)
    (when handler
      (serialize handler stream))
    (when finalizer
      (format stream "finally ")
      (serialize finalizer stream))))

(define-serialize-method catch-clause (stream)
  (with-slots (param body) catch-clause
    (format stream "catch ")
    (format stream "(")
    (serialize param stream)
    (format stream ")")
    (serialize body stream)))

(define-serialize-method while-statement (stream)
  (with-slots (test body) while-statement
    (format stream "while ")
    (format stream "(")
    (serialize test stream)
    (format stream ")")
    (serialize body stream)))

(define-serialize-method do-while-statement (stream)
  (with-slots (test body) do-while-statement
    (format stream "do")
    (serialize body stream)
    (format stream "while")
    (format stream "(")
    (serialize test stream)
    (format stream ")")))

(define-serialize-method for-statement (stream)
  (with-slots (init test update body) for-statement
    (format stream "for ")
    (format stream "(")
    (when init
      (serialize init stream))
    ;; (format stream ";")
    (when test
      (serialize test stream))
    (format stream ";")
    (when update
      (serialize update stream))
    (format stream ")")
    (serialize body stream)))

(define-serialize-method for-in-statement (stream)
  (with-slots (left right body) for-in-statement
    (format stream "for (")
    (serialize left stream)
    (format stream " in ")
    (serialize right stream)
    (format stream ")")
    (serialize body stream)
    (format stream ";")))

(define-serialize-method for-of-statement (stream)
  (with-slots (left right body) for-in-statement
    (format stream "for (")
    (serialize left stream)
    (format stream " of ")
    (serialize right stream)
    (format stream ")")
    (serialize body stream)
    (format stream ";")))

(define-serialize-method function-declaration (stream)
  (with-slots (id params body generator async) function-declaration
    (format stream "function (")
    (loop for first-p = t then nil
          for param in params
          do (unless first-p
               (format stream ", "))
          (serialize param stream))
    (format stream ")")
    (serialize body stream)))

(define-serialize-method variable-declaration (stream)
  (with-slots (declarations kind) variable-declaration
    (format stream kind)
    (format stream " ")
    (loop for first-p = t then nil
          for declaration in declarations
          do (unless first-p
               (format stream ", "))
          (serialize declaration stream))
    (format stream ";")))

(define-serialize-method variable-declarator (stream)
  (with-slots (id init) variable-declarator
    (serialize id stream)
    (when init
      (format stream "=")
      (serialize init stream))))

(define-serialize-method super (stream)
  (format stream "super;"))

(define-serialize-method this-expression (stream)
  (format stream "this"))

(define-serialize-method array-expression (stream)
  (with-slots (elements) array-expression
    (format stream "[")
    (loop for first-p = t then nil
          for element in elements
          do (unless first-p
               (format stream ","))
          (serialize element stream))
    (format stream "]")))

(define-serialize-method object-expression (stream)
  (with-slots (properties) object-expression
    (format stream "{")
    (loop for first-p = t then nil
          for property in properties
          do (unless first-p
               (format stream ","))
          (serialize property stream))
    (format stream "}")))

(define-serialize-method property (stream)
  (with-slots (key value kind) property
    (serialize key stream)
    (format stream ":")
    (serialize value stream)))

(define-serialize-method function-expression (stream)
  (with-slots (params body generator async) function-expression
    (format stream "function(")
    (loop for first-p = t then nil
          for param in params
          do (unless first-p
               (format stream ", "))
          (serialize param stream))
    (format stream ")")
    (serialize body stream)))

(define-serialize-method unary-expression (stream)
  (with-slots (operator prefix argument) unary-expression
    (serialize operator stream)
    (when (> (length operator) 1)
      (format stream " "))
    (serialize argument stream)))

(define-serialize-method update-expression (stream)
  (with-slots (operator prefix argument) update-expression
    (serialize operator stream)
    (serialize argument stream)))

;; paren
(define-serialize-method binary-expression (stream)
  (with-slots (operator left right) binary-expression
    (serialize left stream)
    (format stream " ")
    (serialize operator stream)
    (format stream " ")
    (serialize right stream)))

(define-serialize-method assignment-expression (stream)
  (with-slots (operator left right) assignment-expression
    (serialize left stream)
    (serialize operator stream)
    (serialize right stream)))

;; paren
(define-serialize-method logical-expression (stream)
  (with-slots (operator left right) logical-expression
    (serialize left stream)
    (serialize operator stream)
    (serialize right stream)))

(define-serialize-method member-expression (stream))

(define-serialize-method computed-member-expression (stream)
  (with-slots (object property) computed-member-expression
    (serialize object stream)
    (format stream "[")
    (serialize property stream)
    (format stream "]")))

(define-serialize-method static-member-expression (stream)
  (with-slots (object property) static-member-expression
    (serialize object stream)
    (format stream ".")
    (serialize property stream)))

(define-serialize-method conditional-expression (stream)
  (with-slots (test consequent alternate) conditional-expression
    (serialize test stream)
    (format stream " ? ")
    (serialize consequent stream)
    (format stream " : ")
    (serialize alternate stream)))

(define-serialize-method call-expression (stream)
  (with-slots (callee arguments) call-expression
    (serialize callee stream)
    (format stream "(")
    (loop for first-p = t then nil
          for argument in arguments
          do (unless first-p
               (format stream ", "))
          (serialize argument stream))
    (format stream ")")))

(define-serialize-method new-expression (stream)
  (with-slots (callee arguments) new-expression
    (format stream "new ")
    (serialize callee stream)
    (when arguments
      (format stream "(")
      (loop for first-p = t then nil
            for argument in arguments
            do (unless first-p
                 (format stream ", ")
                 (serialize argument stream)))
      (format stream ")"))))

(define-serialize-method sequence-expression (stream)
  (with-slots (expressions) sequence-expression
    (loop for first-p = t then nil
          for expression in expressions
          do (unless first-p
               (format stream ", "))
          (serialize expression stream))))

(define-serialize-method spread-element (stream)
  (format stream "..."))

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