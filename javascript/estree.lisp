(in-package :javascript)

;; https://github.com/estree/estree/blob/master/es5.md

(defclass node ()
  ((type
    :initarg :type
    :initform nil
    :accessor node-type)
   (loc
    :initarg :loc
    :initform nil
    :accessor node-loc)))

(defclass source-location ()
  ((source
    :initarg :source
    :initform nil
    :accessor source-location-source)
   (start
    :initarg :start
    :initform nil
    :accessor source-location-start)
   (end
    :initarg :end
    :initform nil
    :accessor source-location-end)))

(defclass position ()
  ((line
    :initarg :line
    :initform nil
    :accessor position-line)
   (column
    :initarg :column
    :initform nil
    :accessor position-column)))

(defclass identifier (expression pattern)
  ((type :initform "Identifier")
   (name
    :initarg :name
    :initform nil
    :accessor identifier-name)))

(defclass literal (expression)
  ((type :initform "Literal")
   (value
    :initarg :value
    :initform nil
    :accessor literal-value)))

(defclass reg-exp-literal (literal)
  ((regex-pattern
    :initarg :regex-pattern
    :initform nil
    :accessor reg-exp-literal-regex-pattern)
   (regex-flags
    :initarg :regex-flags
    :initform nil
    :accessor reg-exp-literal-regex-flags)))

(defclass program (node)
  ((type :initform "Program")
   (source-type
    :initarg :source-type
    :initform nil
    :accessor program-source-type)
   (body
    :initarg :body
    :initform nil
    :accessor program-body)))

(defclass function (node)
  ((id
    :initarg :id
    :initform nil
    :accessor function-id)
   (params
    :initarg :params
    :initform nil
    :accessor function-params)
   (body
    :initarg :body
    :initform nil
    :accessor function-body)
   (generator
    :initarg :generator
    :initform nil
    :accessor function-generator)))

(defclass statment (node) ())

(defclass expression-statment (statment)
  ((type :initform "ExpressionStatement")
   (expression
    :initarg :expression
    :initform nil
    :accessor expression-statment-expression)))

(defclass directive (node)
  ((type :initform "ExpressionStatement")
   (expression
    :initarg :expression
    :initform nil
    :accessor directive-expression)
   (directive
    :initarg :expression
    :initform nil
    :accessor directive-directive)))

(defclass block-statement (statement)
  ((type :initform "BlockStatement")
   (body
    :initarg :body
    :initform nil
    :accessor block-statement-body)))

(defclass function-body (block-statement)
  ((body
    :initarg :body
    :initform nil
    :accessor function-body-body)))

(defclass empty-statement (statement)
  ((type :initform "EmptyStatement")))

(defclass debugger-statement (statement)
  ((type :initform "DebuggerStatement")))

(defclass with-statement (statement)
  ((type :initform "WithStatement")
   (object
    :initarg :object
    :initform nil
    :accessor with-statement-object)
   (body
    :initarg :body
    :initform nil
    :accessor with-statement-body)))

(defclass return-statement (statement)
  ((type :initform "ReturnStatement")
   (argument
    :initarg :argument
    :initform nil
    :accessor return-statement-argument)))

(defclass labeled-statement (statement)
  ((type :initform "LabeledStatement")
   (label
    :initarg :label
    :initform nil
    :accessor labeled-statement-label)
   (body
    :initarg :body
    :initform nil
    :accessor labeled-statement-body)))

(defclass break-statement (statement)
  ((type :initform "BreakStatement")
   (label
    :initarg :label
    :initform nil
    :accessor break-statement-label)))

(defclass continue-statement (statement)
  ((type :initform "ContinueStatement")
   (label
    :initarg :label
    :initform nil
    :accessor continue-statement-label)))

(defclass if-statement (statement)
  ((type :initform "IfStatement")
   (test
     :initarg :test
     :initform nil
     :accessor if-statement-test)
   (consequent
    :initarg :consequent
    :initform nil
    :accessor if-statement-consequent)
   (alternate
    :initarg :alternate
    :initform nil
    :accessor if-statement-alternate)))

(defclass switch-statement (statement)
  ((type :initform "SwitchStatement")
   (discriminant
    :initarg :discriminant
    :initform nil
    :accessor switch-statement-discriminant)
   (cases
    :initarg :cases
    :initform nil
    :accessor switch-statement-cases)))

(defclass switch-case (node)
  ((type :initform "SwitchCase")
   (test
    :initarg :test
    :initform nil
    :accessor switch-case-test)
   (consequent
    :initarg :consequent
    :initform nil
    :accessor switch-case-consequent)))

(defclass throw-statement (statement)
  ((type :initform "ThrowStatement")
   (argument
    :initarg :argument
    :initform nil
    :accessor throw-statement-argument)))

(defclass try-statement (statement)
  ((type :initform "TryStatement")
   (block
    :initarg :block
    :initform nil
    :accessor try-statement-block)
   (handler
    :initarg :handler
    :initform nil
    :accessor try-statement-handler)
   (finalizer
    :initarg :finalizer
    :initform nil
    :accessor try-statement-finalizer)))

(defclass catch-clause (node)
  ((type :initform "CatchClause")
   (param
    :initarg :param
    :initform nil
    :accessor catch-clause-param)
   (body
    :initarg :body
    :initform nil
    :accessor catch-clause-body)))

(defclass while-statement (statement)
  ((type :initform "WhileStatement")
   (test
    :initarg :test
    :initform nil
    :accessor while-statement-test)
   (body
    :initarg :body
    :initform nil
    :accessor while-statement-body)))

(defclass do-while-statement (statement)
  ((type :initform "DoWhileStatement")
   (body
    :initarg :body
    :initform nil
    :accessor do-while-statement-body)
   (test
    :initarg :test
    :initform nil
    :accessor do-while-statement-test)))

(defclass for-statement (statement)
  ((type :initform "ForStatement")
   (init
    :initarg :init
    :initform nil
    :accessor for-statement-init)
   (test
    :initarg :test
    :initform nil
    :accessor for-statement-test)
   (update
    :initarg :update
    :initform nil
    :accessor for-statement-update)
   (body
    :initarg :body
    :initform nil
    :accessor for-statement-body)))

(defclass for-in-statement (statement)
  ((type :initform "ForInStatement")
   (left
    :initarg :left
    :initform nil
    :accessor for-in-statement-left)
   (right
    :initarg :right
    :initform nil
    :accessor for-in-statement-right)
   (body
    :initarg :body
    :initform nil
    :accessor for-in-statement-body)))

(defclass for-of-statement (for-in-statement)
  ((type :initform "ForOfStatement")))

(defclass declaration (statement) ())

(defclass function-declaration (function declaration)
  ((type :initform "FunctionDeclaration")
   (id
    :initarg :id
    :initform nil
    :accessor function-declaration-id)))

(defclass variable-declaration (declaration)
  ((type :initform "VariableDeclaration")
   (declarations
    :initarg :declarations
    :initform nil
    :accessor variable-declaration-declarations)
   (kind
    :initarg :kind
    :initform nil ;; var | let | const
    :accessor variable-declaration-kind)))

(defclass variable-declarator (node)
  ((type :initform "VariableDeclarator")
   (id
    :initarg :id
    :initform nil
    :accessor variable-declarator-id)
   (init
    :initarg :init
    :initform nil
    :accessor variable-declarator-init)))

(defclass super (node)
  ((type :initform "Super")))

(defclass expression (node) ())

(defclass this-expression (expression)
  ((type :initform "ThisExpression")))

(defclass array-expression (expression)
  ((type :initform "ArrayExpression")
   (elements
    :initarg :elements
    :initform nil
    :accessor array-expression-elements)))

(defclass object-expression (expression)
  ((type :initform "ObjectExpression")
   (properties
    :initarg :properties
    :initform nil
    :accessor object-expression-properties)))

(defclass property (node)
  ((type :initform "Property")
   (key
    :initarg :key
    :initform nil
    :type (or literal identifier)
    :accessor property-key)
   (value
    :initarg :key
    :initform nil
    :type expression
    :accessor property-value)
   (kind
    :initarg :kind
    :initform nil
    :accessor property-kind)))

(defclass function-expression (function expression)
  ((type :initform "FunctionExpression")))

(defclass unary-expression (expression)
  ((type :initform "UnaryExpression")
   (operator
    :initarg :operator
    :initform nil
    :accessor unary-expression-operator)
   (prefix
    :initarg :prefix
    :initform nil
    :accessor unary-expression-prefix)
   (argument
    :initarg :argument
    :initform nil
    :accessor unary-expression-argument)))

(defclass unary-operator () ())

(defclass update-expression (expression)
  ((type :initform "UpdateExpression")
   (operator
    :initarg :operator
    :initform nil
    :accessor update-expression-operator)
   (argument
    :initarg :argument
    :initform nil
    :accessor update-expression-argument)
   (prefix
    :initarg :prefix
    :initform nil
    :accessor update-expression-prefix)))

(defclass update-operator () ())

(defclass binary-expression ()
  ((type :initform "BinaryExpression")
   (operator
    :initarg :operator
    :initform nil
    :accessor binary-expression-operator)
   (left
    :initarg :left
    :initform nil
    :accessor binary-expression-left)
   (right
    :initarg :right
    :initform nil
    :accessor binary-expression-right)))

(defclass binary-operator () ())

(defclass assignment-expression ()
  ((type :initform "AssignmentExpression")
   (operator
    :initarg :operator
    :initform nil
    :accessor assignment-expression-operator)
   (left
    :initarg :left
    :initform nil
    :accessor assignment-expression-left)
   (right
    :initarg :right
    :initform nil
    :accessor assignment-expression-right)))

(defclass assignment-operator () ())

(defclass logical-expression ()
  ((type :initform "LogicalExpression")
   (operator
    :initarg :operator
    :initform nil
    :accessor logical-expression-operator)
   (left
    :initarg :left
    :initform nil
    :accessor logical-expression-left)
   (right
    :initarg :right
    :initform nil
    :accessor logical-expression-right)))

(defclass logical-operator () ())

(defclass member-expression (expression pattern)
  ((type :initform "MemberExpression")
   (object
    :initarg :object
    :initform nil
    :accessor member-expression-object)
   (property
    :initarg :property
    :initform nil
    :accessor member-expression-property)
   (computed
    :initarg :computed
    :initform nil
    :accessor member-expression-computed)))

(defclass conditional-expression (expression)
  ((type :initform "MemberExpression")
   (test
    :initarg :test
    :initform nil
    :accessor conditional-expression-test)
   (alternate
    :initarg :alternate
    :initform nil
    :accessor conditional-expression-alternate)
   (consequent
    :initarg :consequent
    :initform nil
    :accessor conditional-expression-consequent)))

(defclass call-expression (expression)
  ((type :initform "CallExpression")
   (callee
    :initarg :callee
    :initform nil
    :accessor call-expression-callee)
   (arguments
    :initarg :arguments
    :initform nil
    :accessor call-expression-arguments)))

(defclass new-expression (expression)
  ((type :initform "NewExpression")
   (callee
    :initarg :callee
    :initform nil
    :accessor new-expression-callee)
   (arguments
    :initarg :arguments
    :initform nil
    :accessor new-expression-arguments)))

(defclass sequence-expression (expression)
  ((type :initform "SequenceExpression")
   (expressions
    :initarg :expressions
    :initform nil
    :accessor sequence-expression-expressions)))

(defclass spread-element (node)
  ((type :initform "SpreadElement")
   (argument
    :initarg :argument
    :initform nil
    :accessor spread-element-argument)))

(defclass arrow-function-expression (function expression)
  ((type :initform "ArrowFunctionExpression")
   (body
    :initarg :body
    :initform nil
    :accessor arrow-function-expression-body)
   (expression
    :initarg :expression
    :initform nil
    :accessor arrow-function-expression-expression)))

(defclass yield-expression (expression)
  ((type :initform "YieldExpression")
   (argument
    :initarg :argument
    :initform nil
    :accessor yield-expression-argument)
   (delegate
    :initarg :delegate
    :initform nil
    :accessor yield-expression-delegate)))

(defclass template-literal (expression)
  ((type :initform "TemplateLiteral")
   (quasis
    :initarg :quasis
    :initform nil
    :accessor template-literal-quasis)
   (expressions
    :initarg :expressions
    :initform nil
    :accessor template-literal-expressions)))

(defclass tagged-template-expression (expression)
  ((type :initform "TaggedTemplateExpression")
   (tag
    :initarg :tag
    :initform nil
    :accessor tagged-template-expression-tag)
   (quasi
    :initarg :quasi
    :initform nil
    :accessor tagged-template-expression-quasi)))

(defclass template-element (node)
  ((type :initform "TemplateElement")
   (tail
    :initarg :tail
    :initform nil
    :accessor template-element-tail)
   (value-cooked
    :initarg :value-cooked
    :initform nil
    :accessor template-element-value-cooked)
   (value-raw
    :initarg :valur-raw
    :initform nil
    :accessor template-element-value-raw)))

(defclass assignment-property (property)
  ((type :initform "Property")
   (value
    :initarg :value
    :initform nil
    :accessor assignment-property-value)
   (kind
    :initarg :kind
    :initform nil
    :accessor assignment-property-kind)
   (method
    :initarg :method
    :initform nil
    :accessor assignment-property-method)))

(defclass pattern (node) ())

(defclass object-pattern (pattern)
  ((type :initform "ObjectPattern")
   (properties
    :initarg :properties
    :initform nil
    :accessor object-pattern-properties)))

(defclass array-pattern (pattern)
  ((type :initform "ArrayPattern")
   (elements
    :initarg :elements
    :initform nil
    :accessor array-pattern-elements)))

(defclass rest-element (pattern)
  ((type :initform "RestElement")
   (argument
    :initarg :argument
    :initform nil
    :accessor rest-element-argument)))

(defclass assignment-pattern (pattern)
  ((type :initform "AssignmentPattern")
   (left
    :initarg :left
    :initform nil
    :accessor assignment-pattern-type)
   (right
    :initarg :right
    :initform nil
    :accessor assignment-pattern-right)))

(defclass class (node)
  ((id
    :initarg :id
    :initform nil
    :accessor class-id)
   (super-class
    :initarg :super-class
    :initform nil
    :accessor class-super-class)
   (body
    :initarg :body
    :initform nil
    :accessor class-body)))

(defclass class-body (node)
  ((type :initform "ClassBody")
   (body
    :initarg :body
    :initform nil
    :accessor class-body-body)))

(defclass method-definition (node)
  ((type :initform "MethodDefinition")
   (key
    :initarg :key
    :initform nil
    :accessor method-definition-key)
   (value
    :initarg :value
    :initform nil
    :accessor method-definition-value)
   (kind
    :initarg :kind
    :initform nil
    :accessor method-definition-kind)
   (computed
    :initarg :computed
    :initform nil
    :accessor method-definition-computed)
   (static
    :initarg :static
    :initform nil
    :accessor method-definition-static)))

(defclass class-declaration (class declaration)
  ((type :initform "ClassDeclaration")
   (id
    :initarg :id
    :initform nil
    :accessor class-declaration-id)))

(defclass class-expression (class expression)
  ((type :initform "ClassExpression")))

(defclass meta-property (expression)
  ((type :initform "MetaProperty")
   (meta
    :initarg :meta
    :initform nil
    :accessor meta-property-meta)
   (property
    :initarg :property
    :initform nil
    :accessor meta-property-property)))

(defclass module-declaration (node) ())

(defclass module-specifier (node)
  ((local
    :initarg :local
    :initform nil
    :accessor module-specifier-local)))

(defclass import-declaration (module-declaration)
  ((type :initform "ImportDeclaration")
   (specifiers
    :initarg :specifiers
    :initform nil
    :accessor import-declaration-specifiers)
   (source
    :initarg :source
    :initform nil
    :accessor import-declaration-source)))

(defclass import-specifier (module-specifier)
  ((type :initform "ImportSpecifier")
   (imported
    :initarg :imported
    :initform nil
    :accessor import-specifier-imported)))

(defclass import-default-specifier (module-specifier)
  ((type :initform "ImportDefaultSpecifier")))

(defclass import-namespace-specifier (module-specifier)
  ((type :initform "ImportNamespaceSpecifier")))

(defclass export-named-declaration (module-declaration)
  ((type :initform "ExportNamedDeclaration")
   (declaration
    :initarg :declaration
    :initform nil
    :accessor export-named-declaration-declaration)
   (specifiers
    :initarg :specifiers
    :initform nil
    :accessor export-named-declaration-specifiers)
   (source
    :initarg :source
    :initform nil
    :accessor export-named-declaration-source)))

(defclass export-specifier (module-specifier)
  ((type :initform "ExportSpecifier")
   (exported
    :initarg :exported
    :initform nil
    :accessor export-specifier-exported)))

(defclass anonymous-default-exported-function-declaration (function)
  ((type :initform "FunctionDeclaration")
   (id
    :initarg :id
    :initform nil
    :accessor anonymous-default-exported-function-declaration-id)))

(defclass anonymous-default-exported-class-declaration (functioclass)
  ((type :initform "ClassDeclaration")
   (id
    :initarg :id
    :initform nil
    :accessor anonymous-default-exported-class-declaration-id)))

(defclass export-default-declaration (module-declaration)
  ((type :initform "ExportDefaultDeclaration")
   (declaration
    :initarg :declaration
    :initform nil
    :accessor export-default-declaration-declaration)))

(defclass export-all-declaration (module-declaration)
  ((type :initform "ExportAllDeclaration")
   (source
    :initarg :source
    :initform nil
    :accessor export-all-declaration-source)))
