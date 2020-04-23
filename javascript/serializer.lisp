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

(defparameter *serialize-pretty* t)

(defparameter *serialize-indent* 0)

(defparameter *serialize-offset* 0)

(defun write-indentation (stream)
  (when *serialize-pretty*
    (loop repeat *serialize-indent*
          do (loop repeat 4
                   do (write-whitespace stream))
          finally (loop repeat *serialize-offset*
                        do (write-whitespace stream)))))

(defun write-newline (stream)
  (when *serialize-pretty*
    (write-char #\Newline stream)))

(defun write-whitespace (stream &optional force)
  (if force
      (write-char #\space stream)
    (when *serialize-pretty*
      (write-char #\space stream))))

(defmacro with-indent (&body body)
  `(let ((*serialize-indent* (1+ *serialize-indent*)))
     ,@body))

(defmacro with-offset (offset &body body)
  `(let ((*serialize-offset* ,offset))
     ,@body))

(define-serialize-method string (stream)
  (format stream "~A" string))

(define-serialize-method identifier (stream)
  (format stream "~A" (identifier-name identifier)))

(define-serialize-method keyword (stream)
  (format stream "~A" (keyword-name keyword)))

(define-serialize-method boolean-literal (stream)
  (format stream "~A" (literal-value boolean-literal)))

(define-serialize-method null-literal (stream)
  (write-string "null" stream))

(define-serialize-method numeric-literal (stream)
  (format stream "~A" (literal-value numeric-literal)))

(define-serialize-method string-literal (stream)
  (format stream "~S" (literal-value string-literal)))

(define-serialize-method regular-expression-literal (stream)
  (with-slots (pattern flags) regular-expression-literal
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
          for first-p = t then nil
          do (unless first-p
               (write-newline stream)
               (write-newline stream))
          (serialize node stream))))

(define-serialize-method function (stream))

(define-serialize-method expression-statement (stream)
  (with-slots (expression) expression-statement
    (serialize expression stream)
    (write-char #\; stream)))

(define-serialize-method block-statement (stream)
  (with-slots (body) block-statement
    (write-char #\{ stream)
    (loop for node in (ensure-list body)
          do (write-newline stream)
          (with-indent
            (write-indentation stream)
            (serialize node stream)))
    (write-newline stream)
    (write-indentation stream)
    (write-char #\} stream)))

(define-serialize-method empty-statement (stream)
  (write-char #\; stream))

(define-serialize-method debugger-statement (stream)
  (write-string "debugger" stream)
  (write-char #\; stream))

(define-serialize-method with-statement (stream))

(define-serialize-method return-statement (stream)
  (with-slots (argument) return-statement
    (write-string "return" stream)
    (when argument
      (write-whitespace stream t)
      (serialize argument stream))
    (write-char #\; stream)))

(define-serialize-method labeled-statement (stream))

(define-serialize-method break-statement (stream)
  (with-slots (label) break-statement
    (write-string "break" stream)
    (when label
      (write-whitespace stream t)
      (serialize label stream))
    (write-char #\; stream)))

(define-serialize-method continue-statement (stream)
  (with-slots (label) continue-statement
    (write-string "continue" stream)
    (when label
      (write-whitespace stream t)
      (serialize label stream))
    (write-char #\; stream)))

(define-serialize-method if-statement (stream)
  (with-slots (test consequent alternate) if-statement
    (write-string "if" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize test stream)
    (write-char #\) stream)
    (write-whitespace stream)
    (serialize consequent stream)
    (when alternate
      (write-whitespace stream)
      (write-string "else" stream)
      (write-whitespace stream)
      (serialize alternate stream))))

(define-serialize-method switch-statement (stream)
  (with-slots (discriminant cases) switch-statement
    (write-string "switch" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize discriminant stream)
    (write-char #\) stream)
    (write-char #\{ stream)
    (loop for case in cases
          do (serialize case stream))
    (write-char #\} stream)))

(define-serialize-method switch-case (stream)
  (with-slots (test consequent) switch-case
    (if test
        (progn
          (write-string "case" stream)
          (write-whitespace stream)
          (serialize test stream)
          (write-char #\: stream))
      (progn
        (write-string "default" stream)
        (write-char #\: stream)))
    (loop for node in consequent
          do (serialize node stream))))

(define-serialize-method throw-statement (stream)
  (with-slots (argument) throw-statement
    (write-string "throw" stream)
    (write-whitespace stream)
    (serialize argument stream)))

(define-serialize-method try-statement (stream)
  (with-slots (block handler finalizer) try-statement
    (write-string "try" stream)
    (write-whitespace stream)
    (serialize block stream)
    (when handler
      (serialize handler stream))
    (when finalizer
      (write-string "finally" stream)
      (write-whitespace stream)
      (serialize finalizer stream))))

(define-serialize-method catch-clause (stream)
  (with-slots (param body) catch-clause
    (write-whitespace stream)
    (write-string "catch" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize param stream)
    (write-char #\) stream)
    (write-whitespace stream)
    (serialize body stream)))

(define-serialize-method while-statement (stream)
  (with-slots (test body) while-statement
    (write-string "while" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize test stream)
    (write-char #\) stream)
    (serialize body stream)))

(define-serialize-method do-while-statement (stream)
  (with-slots (test body) do-while-statement
    (write-string "do" stream)
    (write-whitespace stream)
    (serialize body stream)
    (write-whitespace stream)
    (write-string "while" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize test stream)
    (write-char #\) stream)))

(define-serialize-method for-statement (stream)
  (with-slots (init test update body) for-statement
    (write-string "for" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (when init
      (serialize init stream))
    ;; (write-char #\; stream)
    (when test
      (serialize test stream))
    (write-char #\; stream)
    (write-whitespace stream)
    (when update
      (serialize update stream))
    (write-char #\) stream)
    (serialize body stream)))

(define-serialize-method for-in-statement (stream)
  (with-slots (left right body) for-in-statement
    (write-string "for" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize left stream)
    (write-whitespace stream t)
    (write-string "in" stream)
    (write-whitespace stream t)
    (serialize right stream)
    (write-char #\) stream)
    (serialize body stream)
    (write-char #\; stream)))

(define-serialize-method for-of-statement (stream)
  (with-slots (left right body) for-of-statement
    (write-string "for" stream)
    (write-whitespace stream)
    (write-char #\( stream)
    (serialize left stream)
    (write-whitespace stream t)
    (write-string "of" stream)
    (write-whitespace stream t)
    (serialize right stream)
    (write-char #\) stream)
    (serialize body stream)
    (write-char #\; stream)))

(define-serialize-method function-declaration (stream)
  (with-slots (id params body generator async) function-declaration
    (write-string "function" stream)
    (write-whitespace stream t)
    (serialize id stream)
    (write-char #\( stream)
    (loop for param in params
          for first-p = t then nil
          do (unless first-p
               (write-char #\, stream)
               (write-whitespace stream))
          (serialize param stream))
    (write-char #\) stream)
    (write-whitespace stream)
    (serialize body stream)))

(define-serialize-method variable-declaration (stream)
  (with-slots (declarations kind) variable-declaration
    (write-string kind stream)
    (write-whitespace stream t)
    (loop for declaration in declarations
          for first-p = t then nil
          do (unless first-p
               (write-char #\, stream)
               (write-newline stream)
               (with-offset (1+ (length kind))
                 (write-indentation stream)))
          (serialize declaration stream))
    (write-char #\; stream)))

(define-serialize-method variable-declarator (stream)
  (with-slots (id init) variable-declarator
    (serialize id stream)
    (when init
      (write-whitespace stream)
      (write-char #\= stream)
      (write-whitespace stream)
      (serialize init stream))))

(define-serialize-method super (stream)
  (write-string "super" stream))

(define-serialize-method this-expression (stream)
  (write-string "this" stream))

(define-serialize-method array-expression (stream)
  (with-slots (elements) array-expression
    (write-char #\[ stream)
    (when elements
      (with-indent
        (write-newline stream)
        (write-indentation stream)
        (loop for element in elements
              for first-p = t then nil
              do (unless first-p
                   (write-char #\, stream)
                   (write-whitespace stream))
              (serialize element stream)))
      (write-newline stream)
      (write-indentation stream))    
    (write-char #\] stream)))

(define-serialize-method object-expression (stream)
  (with-slots (properties) object-expression
    (write-char #\{ stream)
    (when properties
      (with-indent
        (write-newline stream)
        (write-indentation stream)
        (loop for property in properties
              for first-p = t then nil
              do (unless first-p
                   (write-char #\, stream)
                   (write-newline stream)
                   (write-indentation stream))
              (serialize property stream)))
      (write-newline stream)
      (write-indentation stream))
    (write-char #\} stream)))

(define-serialize-method property (stream)
  (with-slots (key value kind) property
    (serialize key stream)
    (write-char #\: stream)
    (write-whitespace stream)
    (serialize value stream)))

(define-serialize-method function-expression (stream)
  (with-slots (params body generator async) function-expression
    (write-string "function" stream)
    (write-char #\( stream)
    (loop for first-p = t then nil
          for param in params
          do (unless first-p
               (write-char #\, stream)
               (write-whitespace stream))
          (serialize param stream))
    (write-char #\) stream)
    (write-whitespace stream)
    (serialize body stream)))

(define-serialize-method unary-expression (stream)
  (with-slots (operator argument) unary-expression
    (serialize operator stream)
    (when (> (length operator) 1)
      (write-whitespace stream))
    (serialize argument stream)))

(define-serialize-method update-expression (stream)
  (with-slots (prefix operator argument) update-expression
    (if prefix
        (progn
          (serialize operator stream)
          (serialize argument stream))
      (progn
        (serialize argument stream)
        (serialize operator stream)))))

(define-serialize-method binary-expression (stream)
  (with-slots (operator left right) binary-expression
    (if (and (typep left 'binary-expression)
             (< (operator-precedence (slot-value left 'operator))
                (operator-precedence operator)))
        (progn
          (write-char #\( stream)
          (serialize left stream)
          (write-char #\) stream))
      (serialize left stream))
    (if (member operator '("in" "instanceof") :test 'equal)
        (write-whitespace stream t)
      (write-whitespace stream))
    (serialize operator stream)
    (if (member operator '("in" "instanceof") :test 'equal)
        (write-whitespace stream t)
      (write-whitespace stream))
    (if (and (typep right 'binary-expression)
             (< (operator-precedence (slot-value right 'operator))
                (operator-precedence operator)))
        (progn
          (write-char #\( stream)
          (serialize right stream)
          (write-char #\) stream))
      (serialize right stream))))

(define-serialize-method assignment-expression (stream)
  (with-slots (operator left right) assignment-expression
    (serialize left stream)
    (write-whitespace stream)
    (serialize operator stream)
    (write-whitespace stream)
    (serialize right stream)))

(define-serialize-method member-expression (stream))

(define-serialize-method computed-member-expression (stream)
  (with-slots (object property) computed-member-expression
    (serialize object stream)
    (write-char #\[ stream)
    (serialize property stream)
    (write-char #\] stream)))

(define-serialize-method static-member-expression (stream)
  (with-slots (object property) static-member-expression
    (serialize object stream)
    (write-char #\. stream)
    (serialize property stream)))

(define-serialize-method conditional-expression (stream)
  (with-slots (test consequent alternate) conditional-expression
    (serialize test stream)
    (write-whitespace stream t)
    (write-char #\? stream)
    (write-whitespace stream t)
    (serialize consequent stream)
    (write-whitespace stream t)
    (write-char #\: stream)
    (write-whitespace stream t)
    (serialize alternate stream)))

(define-serialize-method call-expression (stream)
  (with-slots (callee arguments) call-expression
    (serialize callee stream)
    (write-char #\( stream)
    (loop for argument in arguments
          for first-p = t then nil
          do (unless first-p
               (write-char #\, stream)
               (write-whitespace stream))
          (serialize argument stream))
    (write-char #\) stream)))

;; https://tc39.es/ecma262/#sec-new-operator
(define-serialize-method new-expression (stream)
  (with-slots (callee arguments) new-expression
    (write-string "new" stream)
    (write-whitespace stream t)
    (serialize callee stream)
    (write-char #\( stream)
    (when arguments
      (loop for argument in arguments
            for first-p = t then nil
            do (unless first-p
                 (write-char #\, stream)
                 (write-whitespace stream)
                 (serialize argument stream))))
    (write-char #\) stream)))

(define-serialize-method sequence-expression (stream)
  (with-slots (expressions) sequence-expression
    (loop for first-p = t then nil
          for expression in expressions
          do (unless first-p
               (write-char #\, stream)
               (write-whitespace stream))
          (serialize expression stream))))

(define-serialize-method spread-element (stream)
  (write-string "..." stream))

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