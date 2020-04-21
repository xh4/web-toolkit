(in-package :javascript)

;; https://github.com/estree/estree/blob/master/es5.md

(defclass node ()
  ((location
    :initarg :location
    :initform nil
    :accessor node-location)
   (range
    :initarg :range
    :initform '(nil nil)
    :accessor node-range)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (loop with first-p = t
          for slot in (class-slots (class-of node))
          for slot-name = (slot-definition-name slot)
          for slot-value = (slot-value node slot-name)
          when (and slot-value (not (member slot-name '(location range))))
          do (if first-p
                 (setf first-p nil)
               (write-char #\space stream))
          and do (format stream "~A: ~S" slot-name slot-value))
    (when-let* ((location (node-location node))
                (start (location-start location))
                (end (location-end location)))
      (format stream " [~D:~D-~D:~D]"
              (position-line start)
              (position-column start)
              (position-line end)
              (position-column end)))))

(defclass source-location ()
  ((source
    :initarg :source
    :initform nil
    :accessor location-source)
   (start
    :initarg :start
    :initform nil
    :accessor location-start)
   (end
    :initarg :end
    :initform nil
    :accessor location-end)))

(defmethod print-object ((location source-location) stream)
  (print-unreadable-object (location stream :type t)
    (when-let* ((start (location-start location))
                (end (location-end location)))
      (format stream "[~D:~D-~D:~D]"
              (position-line start)
              (position-column start)
              (position-line end)
              (position-column end)))))

(defclass position ()
  ((line
    :initarg :line
    :initform nil
    :accessor position-line)
   (column
    :initarg :column
    :initform nil
    :accessor position-column)))

(defmethod print-object ((position position) stream)
  (print-unreadable-object (position stream :type t)
    (when-let* ((line (position-line position))
                (column (position-column position)))
      (format stream "[~D:~D]" line column))))

(defclass token (node)
  ((start
    :initarg :start
    :initform nil)
   (end
    :initarg :end
    :initform nil)
   (line-start
    :initarg :line-start
    :initform nil)
   (line-number
    :initarg :line-number
    :initform nil)))

(defmethod initialize-instance :after ((token token) &key)
  (with-slots (start end range) token
    (when (and start end)
      (setf range `(,start ,end)))))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (loop with first-p = t
          for slot in (class-slots (class-of token))
          for slot-name = (slot-definition-name slot)
          for slot-value = (slot-value token slot-name)
          when (and slot-value
                    (not (member slot-name
                                 '(location range start end line-start line-number))))
          do (if first-p
                 (setf first-p nil)
               (write-char #\space stream))
          and do (format stream "~A: ~S" slot-name slot-value))
    (with-slots (start end) token
      (when (and start end)
        (format stream " [~D-~D]" start end)))))

(defgeneric token-value (token))

(defclass eof (token) ())

(defmethod token-value ((eof eof)))

(defclass identifier (token expression pattern)
  ((name
    :initarg :name
    :initform nil
    :accessor identifier-name)))

(defmethod token-value ((identifier identifier))
  (identifier-name identifier))

(defclass literal (expression)
  ((value
    :initarg :value
    :initform nil
    :accessor literal-value)))

(defclass boolean-literal (token literal) ())

(defmethod token-value ((boolean-literal boolean-literal))
  (literal-value boolean-literal))

(defclass null-literal (token literal) ())

(defmethod token-value ((null-literal null-literal))
  (literal-value null-literal))

(defclass numeric-literal (token literal)
  ((octal
    :initarg :octal
    :initform nil)))

(defmethod token-value ((numeric-literal numeric-literal))
  (literal-value numeric-literal))

(defclass string-literal (token literal)
  ((octal
    :initarg :octal
    :initform nil)))

(defmethod token-value ((string-literal string-literal))
  (literal-value string-literal))

;; maybe rename it
(defclass reg-exp-literal (token literal)
  ((pattern
    :initarg :pattern
    :initform nil)
   (flags
    :initarg :flags
    :initform nil)))

(defmethod token-value ((reg-exp-literal reg-exp-literal))
  (literal-value reg-exp-literal))

(defclass keyword (token)
  ((name
    :initarg :name
    :initform nil
    :accessor keyword-name)))

(defmethod token-value ((keyword keyword))
  (keyword-name keyword))

(defclass punctuator (token)
  ((value
    :initarg :value
    :initform nil
    :accessor punctuator-value)))

(defmethod token-value ((punctuator punctuator))
  (punctuator-value punctuator))

(defclass program (node)
  ((source-type
    :initarg :source-type
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass module (program)
  ((source-type
    :initform :module)))

(defclass script (program)
  ((source-type
    :initform :script)))

;; used?
(defclass function (node)
  ((id
    :initarg :id
    :initform nil)
   (params
    :initarg :params
    :initform nil)
   (body
    :initarg :body
    :initform nil)
   (generator
    :initarg :generator
    :initform nil)
   (async
    :initarg :async
    :initform nil)))

(defclass statement (node) ())

(defclass expression-statement (statement)
  ((expression
    :initarg :expression
    :initform nil)))

;; used?
(defclass directive (node)
  ((expression
    :initarg :expression
    :initform nil)
   (directive
    :initarg :directive
    :initform nil)))

(defclass block-statement (statement)
  ((body
    :initarg :body
    :initform nil)))

;; used?
(defclass function-body (block-statement)
  ((body
    :initarg :body
    :initform nil)))

(defclass empty-statement (statement) ())

(defclass debugger-statement (statement) ())

(defclass with-statement (statement)
  ((object
    :initarg :object
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass return-statement (statement)
  ((argument
    :initarg :argument
    :initform nil)))

;; used?
(defclass labeled-statement (statement)
  ((label
    :initarg :label
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass break-statement (statement)
  ((label
    :initarg :label
    :initform nil)))

(defclass continue-statement (statement)
  ((label
    :initarg :label
    :initform nil)))

(defclass if-statement (statement)
  ((test
     :initarg :test
     :initform nil)
   (consequent
    :initarg :consequent
    :initform nil)
   (alternate
    :initarg :alternate
    :initform nil)))

(defclass switch-statement (statement)
  ((discriminant
    :initarg :discriminant
    :initform nil)
   (cases
    :initarg :cases
    :initform nil)))

(defclass switch-case (node)
  ((test
    :initarg :test
    :initform nil)
   (consequent
    :initarg :consequent
    :initform nil)))

(defclass throw-statement (statement)
  ((argument
    :initarg :argument
    :initform nil)))

(defclass try-statement (statement)
  ((block
    :initarg :block
    :initform nil)
   (handler
    :initarg :handler
    :initform nil)
   (finalizer
    :initarg :finalizer
    :initform nil)))

(defclass catch-clause (node)
  ((param
    :initarg :param
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass while-statement (statement)
  ((test
    :initarg :test
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass do-while-statement (statement)
  ((test
    :initarg :test
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass for-statement (statement)
  ((init
    :initarg :init
    :initform nil)
   (test
    :initarg :test
    :initform nil)
   (update
    :initarg :update
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass for-in-statement (statement)
  ((left
    :initarg :left
    :initform nil)
   (right
    :initarg :right
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass for-of-statement (for-in-statement) ())

(defclass declaration (statement) ())

(defclass function-declaration (function declaration)
  ((id
    :initarg :id
    :initform nil)))

(defclass async-function-declaration (function-declaration)
  ((async :initform t)))

(defclass variable-declaration (declaration)
  ((declarations
    :initarg :declarations
    :initform nil)
   (kind
    :initarg :kind
    :initform nil)))

(defclass variable-declarator (node)
  ((id
    :initarg :id
    :initform nil)
   (init
    :initarg :init
    :initform nil)))

(defclass super (node) ())

(defclass expression (node) ())

(defclass this-expression (expression) ())

(defclass array-expression (expression)
  ((elements
    :initarg :elements
    :initform nil)))

(defclass object-expression (expression)
  ((properties
    :initarg :properties
    :initform nil)))

(defclass property (node)
  ((key
    :initarg :key
    :initform nil)
   (value
    :initarg :value
    :initform nil)
   (kind
    :initarg :kind
    :initform nil)
   (method
    :initarg :method
    :initform nil)
   (shorthand
    :initarg :shorthand
    :initform nil)
   (computed
    :initarg :computed
    :initform nil)))

(defclass function-expression (function expression) ())

(defclass unary-expression (expression)
  ((operator
    :initarg :operator
    :initform nil)
   (prefix
    :initarg :prefix
    :initform nil)
   (argument
    :initarg :argument
    :initform nil)))

;; use it
(defclass unary-operator () ())

(defclass update-expression (expression)
  ((operator
    :initarg :operator
    :initform nil)
   (argument
    :initarg :argument
    :initform nil)
   (prefix
    :initarg :prefix
    :initform nil)))

;; use it
(defclass update-operator () ())

(defclass binary-expression (expression)
  ((operator
    :initarg :operator
    :initform nil)
   (left
    :initarg :left
    :initform nil)
   (right
    :initarg :right
    :initform nil)))

;; use it
(defclass binary-operator () ())

(defclass assignment-expression (expression)
  ((operator
    :initarg :operator
    :initform nil)
   (left
    :initarg :left
    :initform nil)
   (right
    :initarg :right
    :initform nil)))

;; use it
(defclass assignment-operator () ())

(defclass logical-expression (expression)
  ((operator
    :initarg :operator
    :initform nil)
   (left
    :initarg :left
    :initform nil)
   (right
    :initarg :right
    :initform nil)))

;; use it
(defclass logical-operator () ())

(defclass member-expression (expression pattern)
  ((object
    :initarg :object
    :initform nil)
   (property
    :initarg :property
    :initform nil)
   (computed
    :initarg :computed
    :initform nil)))

(defclass computed-member-expression (member-expression)
  ((computed
    :initform t)))

(defclass static-member-expression (member-expression)
  ((computed
    :initform nil)))

(defclass conditional-expression (expression)
  ((test
    :initarg :test
    :initform nil)
   (consequent
    :initarg :consequent
    :initform nil)
   (alternate
    :initarg :alternate
    :initform nil)))

(defclass call-expression (expression)
  ((callee
    :initarg :callee
    :initform nil)
   (arguments
    :initarg :arguments
    :initform nil)))

(defclass new-expression (expression)
  ((callee
    :initarg :callee
    :initform nil)
   (arguments
    :initarg :arguments
    :initform nil)))

(defclass sequence-expression (expression)
  ((expressions
    :initarg :expressions
    :initform nil)))

(defclass spread-element (node)
  ((argument
    :initarg :argument
    :initform nil)))

(defclass arrow-function-expression (function expression)
  ((body
    :initarg :body
    :initform nil)
   (expression
    :initarg :expression
    :initform nil)))

(defclass await-expression (expression)
  ((argument
    :initarg :argument
    :initform nil)))

(defclass yield-expression (expression)
  ((argument
    :initarg :argument
    :initform nil)
   (delegate
    :initarg :delegate
    :initform nil)))

(defclass template-literal (expression)
  ((quasis
    :initarg :quasis
    :initform nil)
   (expressions
    :initarg :expressions
    :initform nil)))

(defclass tagged-template-expression (expression)
  ((tag
    :initarg :tag
    :initform nil)
   (quasi
    :initarg :quasi
    :initform nil)))

(defclass template-element (node)
  ((tail
    :initarg :tail
    :initform nil)
   (value-cooked
    :initarg :value-cooked
    :initform nil)
   (value-raw
    :initarg :value-raw
    :initform nil)))

(defclass assignment-property (property)
  ((value
    :initarg :value
    :initform nil)
   (kind
    :initarg :kind
    :initform nil)
   (method
    :initarg :method
    :initform nil)))

(defclass pattern (node) ())

(defclass object-pattern (pattern)
  ((properties
    :initarg :properties
    :initform nil)))

(defclass array-pattern (pattern)
  ((elements
    :initarg :elements
    :initform nil)))

(defclass rest-element (pattern)
  ((argument
    :initarg :argument
    :initform nil)))

(defclass assignment-pattern (pattern)
  ((left
    :initarg :left
    :initform nil)
   (right
    :initarg :right
    :initform nil)))

(defclass class (node)
  ((id
    :initarg :id
    :initform nil)
   (super-class
    :initarg :super-class
    :initform nil)
   (body
    :initarg :body
    :initform nil)))

(defclass class-body (node)
  ((body
    :initarg :body
    :initform nil)))

(defclass method-definition (node)
  ((key
    :initarg :key
    :initform nil)
   (value
    :initarg :value
    :initform nil)
   (kind
    :initarg :kind
    :initform nil)
   (computed
    :initarg :computed
    :initform nil)
   (static
    :initarg :static
    :initform nil)))

(defclass class-declaration (class declaration)
  ((id
    :initarg :id
    :initform nil)))

(defclass class-expression (class expression) ())

(defclass meta-property (expression)
  ((meta
    :initarg :meta
    :initform nil)
   (property
    :initarg :property
    :initform nil)))

(defclass module-declaration (node) ())

(defclass module-specifier (node)
  ((local
    :initarg :local
    :initform nil)))

(defclass import-declaration (module-declaration)
  ((specifiers
    :initarg :specifiers
    :initform nil)
   (source
    :initarg :source
    :initform nil)))

(defclass import-specifier (module-specifier)
  ((imported
    :initarg :imported
    :initform nil)))

(defclass import-default-specifier (module-specifier) ())

(defclass import-namespace-specifier (module-specifier) ())

(defclass export-named-declaration (module-declaration)
  ((declaration
    :initarg :declaration
    :initform nil)
   (specifiers
    :initarg :specifiers
    :initform nil)
   (source
    :initarg :source
    :initform nil)))

(defclass export-specifier (module-specifier)
  ((exported
    :initarg :exported
    :initform nil)))

(defclass anonymous-default-exported-function-declaration (function)
  ((id
    :initarg :id
    :initform nil)))

(defclass anonymous-default-exported-class-declaration (functioclass)
  ((id
    :initarg :id
    :initform nil)))

(defclass export-default-declaration (module-declaration)
  ((declaration
    :initarg :declaration
    :initform nil)))

(defclass export-all-declaration (module-declaration)
  ((source
    :initarg :source
    :initform nil)))
