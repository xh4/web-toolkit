(in-package :javascript)

(defun operator-precedence (operator))

(defclass parser ()
  ((scanner
    :initarg :scanner
    :initform nil)
   (lookahead
    :initform nil)
   (context
    :initform nil)
   (tokens
    :initform nil)
   (has-line-terminator-p
    :initform nil)))

(defun throw-error (message-format &rest values))

(defun tolerate-error (parser message-format &rest values))

(defun unexpected-token-error (token message))

(defun throw-unexpected-token (token message))

(defun tolerate-unexpected-token (token message))

(defun collect-comments (parser))

(defun get-token-raw (parser token)
  (with-slots (scanner) parser
    (with-slots (source) scanner
      (with-slots (start end) token
        (subseq source start end)))))

(defun convert-token (token))

(defun next-token (parser)
  (with-slots (lookahead scanner context) parser
    (let ((token lookahead))
      (collect-comments parser)
      (let ((next-token (lex scanner)))
        (when (and next-token (getf context :strict-p) (typep next-token 'identifier))
          (when (strict-mode-reserved-word-p (slot-value next-token 'value))
            (change-class next-token 'keyword)))
        (setf lookahead next-token))
      token)))

(defun next-regex-token ())

(defun expect (parser value)
  (let ((token (next-token parser)))
    (when (or (not (typep token 'punctuator))
              (not (equal value (typep token 'value))))
      (error "Unexpected token"))))

(defun expect-comma-separator (parser)
  (expect parser ","))

(defun expect-keyword (parser value)
  (let ((token (next-token parser)))
    (when (or (not (typep token 'keyword))
              (not (equal value (slot-value token 'value))))
      (error "Unexpected token"))))

(defun match (parser value)
  (with-slots (lookahead) parser
    (and (typep lookahead 'punctuator)
         (equal value (slot-value lookahead 'value)))))

(defun match-keyword (parser value)
  (with-slots (lookahead) parser
    (and (typep lookahead 'keyword)
         (equal value (slot-value lookahead 'value)))))

(defun match-contextual-keyword (parser value)
  (with-slots (lookahead) parser
    (and (typep lookahead 'identifier)
         (equal value (slot-value lookahead 'value)))))

(defun match-assign (parser))

(defun isolate-cover-grammar (parser parse-function)
  (with-slots (context) parser
    (let ((previous-binding-element-p (getf context :binding-element-p))
          (previous-assignment-target-p (getf context :assignment-target-p))
          (previous-first-cover-initialized-name-error
           (getf context :first-cover-initialized-name-error)))
      (setf (getf context :binding-element-p) t
            (getf context :assignment-target-p) t
            (getf context :first-cover-initialized-name-error) nil)
      (let ((result (funcall parse-function parser)))
        (when (getf context :first-cover-initialized-name-error)
          (error (getf context :first-cover-initialized-name-error)))
        (setf (getf context :binding-element-p) previous-binding-element-p
              (getf context :assignment-target-p) previous-assignment-target-p
              (getf context :first-cover-initialized-name-error)
              previous-first-cover-initialized-name-error)
        result))))

(defun inherit-cover-grammar (parser parse-function)
  (with-slots (context) parser
    (let ((previous-binding-element-p (getf context :binding-element-p))
          (previous-assignment-target-p (getf context :assignment-target-p))
          (previous-first-cover-initialized-name-error (getf context :first-cover-initialized-name-error)))
      (setf (getf context :binding-element-p) t
            (getf context :assignment-target-p) t
            (getf context :first-cover-initialized-name-error) nil)
      (let ((result (funcall parse-function parser)))
        (when (getf context :first-cover-initialized-name-error)
          (error (getf context :first-cover-initialized-name-error)))
        (setf (getf context :binding-element-p)
              (and (getf context :binding-element-p)
                   previous-binding-element-p)
              (getf context :assignment-target-p)
              (and
               (getf context :assignment-target-p) 
               previous-assignment-target-p)
              (getf context :first-cover-initialized-name-error)
              (or (getf context :first-cover-initialized-name-error)
                   previous-first-cover-initialized-name-error))
        result))))

(defun consume-semicolon (parser)
  (with-slots (lookahead) parser
    (cond
     ((match parser ";")
      (next-token parser))
     ((not (has-line-terminator-p parser))
      (when (and (not (typep lookahead 'eof))
                 (not (match parser "}")))
        (throw-unexpected-token parser lookahead))))))

(defun parse-primary-expression (parser)
  (with-slots (lookahead context) parser
    (let ((expression)
          (token)
          (raw))
      (typecase lookahead
        (identifier
         (when (and (or (getf context :module-p)
                        (getf context :await))
                    (equal "await" (slot-value lookahead 'value)))
           (tolerate-unexpected-token parser lookahead))
         (setf expression (if (match-async-function parser)
                              (parse-function-expression parser)
                            (make-instance 'identifier
                                           :name (slot-value (next-token parser) 'value)))))
        ((numeric-literal string-literal)
         (when (and (getf context :strict)
                    (slot-value lookahead 'octal))
           (tolerate-unexpected-token parser lookahead "some message")
           (setf (getf context :assignment-target-p) nil
                 (getf context :binding-elemnt-p) nil
                 token (next-token parser)
                 raw (get-token-raw parser token)
                 expression (make-instance 'literal :value (slot-value token 'value)))))
        (boolean-literal
         (setf (getf context :assignment-target-p) nil
               (getf context :binding-elemnt-p) nil
               token (next-token parser)
               raw (get-token-raw parser token)
               expression (make-instance 'literal
                                         :value (equal "true" (slot-value token 'value)))))
        (null-literal
         (setf (getf context :assignment-target-p) nil
               (getf context :binding-elemnt-p) nil
               token (next-token parser)
               raw (get-token-raw parser token)
               expression (make-instance 'literal)))
        (template
         (setf expression (parse-template-literal parser)))
        (punctuator
         (switch ((slot-value lookahead 'value) :test 'equal)
           ("("
            (setf (getf context :binding-element-p) nil
                  expression (inherit-cover-grammar parser 'parse-group-expression)))
           ("["
            (setf expression (inherit-cover-grammar parser 'parse-array-initializer)))
           ("{"
            (setf expression (inherit-cover-grammar parser 'parse-object-initializer)))
           ((or "/" "/=")
            (setf (getf context :assignment-target-p) nil
                  (getf context :binding-elemnt-p) nil
                  token (next-token parser)
                  raw (get-token-raw parser token)
                  expression (make-instance 'reg-exp-literal)))
           (t (error "Unexpected token"))))
        (keyword
         (cond
          ((and (not (getf context :strict))
                (getf context :allow-yield)
                (match-keyword parser "yield"))
           (setf expression (parse-identifier-name parser)))
          ((and (not (getf context :strict))
                (match-keyword parser "let")))
          (t (setf (getf context :assignment-target-p) nil
                   (getf context :binding-elemnt-p) nil)
             (cond
              ((match-keyword parser "function")
               (setf expression (parse-function-expression parser)))
              ((match-keyword parser "this")
               (next-token parser)
               (setf expression (make-instance 'this-expression)))
              ((match-keyword parser "class")
               (setf expression (parse-class-expression parser)))
              (t (next-token parser) (error "Unexpected token"))))))
        (t (next-token parser) (error "Unexpected token")))
      expression)))

(defun parse-spread-element (parser)
  (expect parser "...")
  (let ((argument (inherit-cover-grammar 'parse-assignment-expression)))
    (make-instance 'spread-element :argument argument)))

(defun parse-array-initializer (parser)
  (with-slots (context) parser
    (let ((elements))
      (expect parser "[")
      (loop while (not (match parser "]"))
            do (cond
                ((match parser ",")
                 (next-token parser)
                 (appendf elements (list nil)))
                ((match parser "...")
                 (let ((element (parse-spread-element parser)))
                   (unless (match parser "]")
                     (setf (getf context :assignment-target-p) nil
                           (getf context :binding-element-p) nil)
                     (expect parser ","))
                   (appendf elements (list element))))
                (t (appendf elements (inherit-cover-grammar parser 'parse-assignment-expression))
                   (unless (match parser "]")
                     (expect parser ",")))))
      (expect parser "]")
      (make-instance 'array-expression :elements elements))))

(defun parse-property-method (parser params))

(defun parse-property-method-function (parser))

(defun parse-property-method-async-function (parser))

(defun parse-object-property-key (parser))

(defun property-key-p (key value))

(defun parse-object-property (parser has-proto))

(defun parse-object-initializer (parser))

(defun parse-template-head (parser))

(defun parse-template-element (parser))

(defun parse-template-literal (parser))

(defun reinterpret-expression-as-pattern (parser argument))

(defun parse-group-expression (parser))

(defun parse-arguments (parser)
  (expect parser "(")
  (let ((arguments))
    (unless (match parser ")")
      (loop
       (let ((expression (if (match parser "...")
                             (parse-spread-element parser)
                           (isolate-cover-grammar 'parse-assignment-expression))))
         (appendf argument (list expression))
         (when (match parser ")")
           (return))
         (expect-comma-separator parser)
         (when (match parser ")")
           (return)))))
    (match parser ")")
    arguments))

(defun identifier-name-p (token)
  (typecase token
    ((identifier keyword boolean-literal null-literal) t)))

(defun parse-identifier-name (parser))

(defun parse-new-expression (parser)
  (with-slots (lookahead context) parser
    (let ((id (parse-identifier-name parser)))
      (let ((expression))
        (if (match parser ".")
            (progn
              (next-token parser)
              (if (and (typep lookahead 'identifier)
                       (getf context :in-function-body)
                       (equal "target" (slot-value lookahead 'value)))
                  (let ((property (parse-identifier-name parser)))
                    (setf expression (make-instance 'meta-property
                                                    :meta id :property property)))
                (error "Unexpected token")))
          (let ((callee (isolate-cover-grammar parser 'parse-left-hand-side-expression))
                (arguments (when (match parser "(")
                             (parse-arguments parser))))
            (setf expression (make-instance 'new-expression
                                            :callee callee
                                            :arguments arguments)
                  (getf context :assignment-target-p) nil
                  (getf context :binding-element-p) nil)))
        expression))))

(defun parse-async-argument (parser)
  (with-slots (context) parser
    (let ((argument (parse-assignment-expression parser)))
      (setf (getf context :first-cover-initialized-name-error) nil)
      argument)))

(defun parse-async-arguments (parser)
  (expect parser "(")
  (let ((arguments))
    (unless (match parser ")")
      (loop
       (let ((expression (if (match parser "...")
                             (parse-spread-element parser)
                           (isolate-cover-grammar parser 'parse-async-argument))))
         (appendf arguments (list expression))
         (when (match parser ")")
           (return))
         (expect-comma-separator parser)
         (when (match parser ")")
           (return)))))
    (expect parser ")")
    arguments))

(defun parse-left-hand-side-expression-allow-call (parser)
  (with-slots (lookahead context) parser
    (let ((start-token lookahead)
          (maybe-async (match-contextual-keyword parser "async"))
          (previous-allow-in (getf context :allow-in))
          (expression))
      (setf (getf context :allow-in) t)
      (if (and (match parser "super")
               (getf context :in-function-body))
          (progn
            (next-token parser)
            (setf expression (make-instance 'super))
            (when (and (not (match parser "("))
                       (not (match parser "."))
                       (not (match parser "[")))
              (error "Unexpected token")))
        (setf expression (inherit-cover-grammar parser
                                                (if (match-keyword parser "new")
                                                    'parse-new-expression
                                                  'parse-primary-expression))))
      (loop
       (cond
        ((match parser ".")
         (setf (getf context :binding-element-p) nil
               (getf context :assignment-target-p) t)
         (expect parser ".")
         (let ((property (parse-identifier-name parser)))
           (setf expression (make-instance 'static-member-expression
                                           :object expression
                                           :property property))))
        ((parse-match parser "(")
         (let ((async-arrow (and maybe-async)))
           (setf (getf context :binding-element-p) nil
                 (getf context :assignment-target-p) nil)
           (let ((arguments (if async-arrow
                                (parse-async-arguments parser)
                              (parse-arguments parser))))
             (setf expression (make-instance 'call-expression
                                             :callee expression
                                             :arguments arguments))
             (when async-arrow
               (match parser "=>")
               (loop for i from 0 upto (1- (length arguments))
                     do (reinterpret-expression-as-pattern parser (nth arguments i)))
               (setf expression `(:type :arrow-parameter-placeholder
                                  :params ,arguments
                                  :async t))))))
        ((match parser "[")
         (setf (getf context :binding-element-p) nil
               (getf context :assignment-target-p) t)
         (expect parser "[")
         (let ((property (isolate-cover-grammar parser 'parse-expression)))
           (expect parser "]")
           (setf expression (make-instance 'computed-member-expression
                                           :object expression
                                           :property property))))
        ((and (typep lookahead 'template)
              (slot-value lookahead 'head))
         (let ((quasi (parse-template-literal parser)))
           (setf expression (make-instance 'tagged-template-expression
                                           :tag expression
                                           :quasi quasi))))
        (t (return))))
      (setf (getf context :allow-in) previous-allow-in)
      expression)))

(defun parse-super (parser)
  (expect-keyword parser "super")
  (when (and (not (match parser "["))
             (not (match parser ".")))
    (error "Unexpected token"))
  (make-instance 'super))

(defun parse-left-hand-side-expression (parser)
  (with-slots (lookahead context) parser
    (let ((expression (if (and (match parser "super")
                               (getf context :in-function-body))
                          (parse-super parser)
                        (inherit-cover-grammar parser
                                               (if (match-keyword parser "new")
                                                   'parse-new-expression
                                                 'parse-primary-expression)))))
      (loop
       (cond
        ((match parser "[")
         (setf (getf context :binding-element-p) nil
               (getf context :assignment-target-p) t)
         (expect parser "[")
         (let ((property (isolate-cover-grammar parser 'parse-expression)))
           (expect parser "]")
           (setf expression (make-instance 'computed-mumber-expression
                                           :object expression
                                           :property property))))
        ((match parser ".")
         (setf (getf context :binding-element-p) nil
               (getf context :assignment-target-p) t)
         (expect parser ".")
         (let ((property (parse-identifier-name parser)))
           (expect parser "]")
           (setf expression (make-instance 'static-member-expression
                                           :object expression
                                           :property property))))
        ((and (typep lookahead 'template)
              (slot-value lookahead 'head))
         (let ((quasi (parse-template-literal parser)))
           (setf expression (make-instance 'tagged-template-expression
                                           :tag expression
                                           :quasi quasi))))
        (t (return))))
      expression)))

(defun parse-update-expression (parser)
  (with-slots (lookahead context) parser
    (let ((expression)
          (start-token lookahead))
      (if (or (match parser "++")
              (match parser "--"))
          (let ((token (next-token parser)))
            (setf expression (inherit-cover-grammar parser 'parser-unary-expression))
            (when (and (getf context :strict)
                       (typep expression 'identifier)
                       (restricted-word-p (slot-value expression 'name)))
              (tolerate-error "some message"))
            (unless (getf context :assignment-target-p)
              (tolerate-error "some message"))
            (let ((prefix t))
              (setf expression (make-instance 'update-expression
                                              :operator (slot-value token 'value)
                                              :argument expression
                                              :prefix prefix)
                    (getf context :assignment-target-p) nil
                    (getf context :binding-element-p) nil)))
        (progn
          (setf expression (inherit-cover-grammar parser 'parse-left-hand-side-expression-allow-call))
          (when (and (typep lookahead 'punctuator))
            (when (or (match parser "++")
                      (match parser "--"))
              (when (and (getf context :strict)
                         (typep expression 'identifier)
                         (restricted-word-p (slot-value expression 'name)))
                (tolerate-error "some message"))
              (unless (getf context :assignment-target-p)
                (tolerate-error "some message"))
              (setf (getf context :assignment-target-p) nil
                    (getf context :binding-element-p) nil)
              (let ((operator (slot-value (next-token parser) 'value))
                    (prefix nil))
                (setf expression (make-instance 'update-expression
                                                :operator operator
                                                :argument expression
                                                :prefix prefix)))))))
      expression)))

(defun parse-await-expression (parser)
  (next-token parser)
  (let ((argument (parse-unary-expression parser)))
    (make-instance 'await-expression :argument argument)))

(defun parse-unary-expression (parser)
  (with-slots (context) parser
    (let ((expression))
      (cond
       ((or (match parser "+")
            (match parser "-")
            (match parser "~")
            (match parser "!")
            (match parser "delete")
            (match parser "void")
            (match parser "typeof"))
        (let ((token (next-token parser)))
          (setf expression (inherit-cover-grammar parser 'parse-unary-expression)
                expression (make-instance 'unary-expression
                                          :operator (slot-value token 'vaue)
                                          :argument expression))
          (when (and (getf context :strict)
                     (equal "delete" (slot-value expression 'operator))
                     (typep (slot-value expression 'argument) 'identifier))
            (tolerate-error parser "some message"))
          (setf (getf context :assignment-target-p) nil
                (getf context :binding-element-p) nil)))
       ((and (getf context :await)
             (match-contextual-keyword parser "await"))
        (setf expression (parse-await-expression parser)))
       (t (setf expression (parse-update-expression parser))))
      expression)))

(defun parse-exponentiation-expression (parser)
  (with-slots (context) parser
    (let ((expression (inherit-cover-grammar parser 'parse-unary-expression)))
      (when (and (not (typep expression 'unary-expression))
                 (match parser "**"))
        (next-token parser)
        (setf (getf context :assignment-target-p) nil
              (getf context :binding-element-p) nil)
        (let ((left expression)
              (right (isolate-cover-grammar parser 'parse-exponentiation-expression)))
          (setf expression (make-instance 'binary-expression
                                          :operator "**"
                                          :left left
                                          :right right))))
      expression)))

(defun binary-precedence (parser token)
  (with-slots (context) parser
    (let ((operator (slot-value token 'value))
          (precedence))
      (cond
       ((typep token 'punctuator)
        (setf precedence (or (operator-precedence operator) 0)))
       ((typep token 'keyword)
        (setf precedence (if (or (equal "instanceof" operator)
                                 (and (getf context :allow-in)
                                      (equal "in" operator)))
                             7
                           0)))
       (t (setf precedence 0)))
      precedence)))

(defun parse-binary-expression (parser))

(defun parse-conditional-expression (parser))

(defun check-pattern-param (parser options param))

(defun reinterpret-as-cover-formals-list (expression))

(defun parse-assignment-expression (parser)
  (with-slots (lookahead context) parser
    (let ((statement))
      (setf (getf context :assgnment-target-p) t
            (getf context :binding-element-p) t)
      (if (typep lookahead 'keyword)
        (switch ((slot-value lookahead 'value) :test 'equal)
          ("export"
           (unless (getf context :module-p)
             (tolerate-unexpected-token parser lookahead "Unexpected token"))
           (setf statement (parser-export-declaration parser)))
          ("import"
           (unless (getf context :module-p)
             (tolerate-unexpected-token parser lookahead "Unexpected token"))
           (setf statement (parser-import-declaration parser)))
          ("const"
           (setf statement (parser-lexical-declaration parser)))
          ("function"
           (setf statement (parser-function-declaration parser)))
          ("class"
           (setf statement (parse-class-declaration parser)))
          ("let"
           (setf statement (if (lexical-declaration-p parser)
                               (parser-lexical-declaration parser)
                             (parse-statement parser))))
          (t (setf statement (parse-statement parser))))
        (setf statement (parse-statement parser)))
      statement)))

(defun parse-expression (parser)
  (with-slots (lookahead) parser 
    (let ((start-token lookahead)
          (expression (isolate-cover-grammar parser 'parse-assignment-expression)))
      (when (match parser ",")
        (let ((expressions `(,expression)))
          (loop while (not (typep lookahead 'eof))
                do (unless (match parser ",")
                     (return))
                (next-token parser)
                (appendf expressions (list (isolate-cover-grammar
                                            parser
                                            'parse-assignment-expression))))
          (setf expression (make-instance 'sequence-expression :expressions expressions))))
      expression)))

(defun parse-statement-list-item (parser))

(defun parse-block (parser))

(defun parse-lexical-binding (parser kind options))

(defun parse-binding-list (parser kind options))

(defun lexical-declaration-p (parser))

(defun parse-lexical-declaration (parser &key in-for))

(defun parse-binding-rest-element (parser params kind))

(defun parse-array-pattern (parser params kind))

(defun parse-property-pattern (parser params kind))

(defun parse-object-pattern (parser params kind))

(defun parse-pattern (parser params kind))

(defun parse-pattern-with-default (parser params kind))

(defun parse-variable-identifier (parser kind))

(defun parse-variable-declaration (parser options))

(defun parse-variable-declaration-list (parser options))

(defun parse-variable-statement (parser))

(defun parse-empty-statement (parser))

(defun parse-expression-statement (parser))

(defun parse-if-clause (parser))

(defun parse-if-statement (parser))

(defun parse-do-while-statement (parser))

(defun parse-while-statement (parser))

(defun parse-for-statement (parser))

(defun parse-continue-statement (parser))

(defun parse-break-statement (parser))

(defun parse-return-statement (parser))

(defun parse-with-statement (parser))

(defun parse-switch-statement (parser))

(defun parse-labelled-statement (parser))

(defun parse-throw-statement (parser))

(defun parse-catch-statement (parser))

(defun parse-finally-clause (parser))

(defun parse-try-statement (parser))

(defun parse-debugger-statement (parser))

(defun parse-statement (parser))

(defun parse-function-source-elements (parser))

(defun validate-param (parser options param name))

(defun parse-rest-element (parser params))

(defun parse-formal-parameter (parser options))

(defun parse-format-parameters (parser &optional first-restricted))

(defun match-async-function (parser))

(defun parse-function-declaration (parser))

(defun parse-function-expression (parser))

(defun parse-directive (parser)
  (with-slots (lookahead) parser
    (let ((token lookahead))
      (let ((expression (parse-expression parser)))
        (let ((directive (when (typep expression 'literal)
                           (let ((raw (get-token-raw parser token)))
                             (subseq raw 1 (1- (length raw)))))))
          (consume-semicolon parser)
          (if directive
              (make-instance 'directive :expression expression :directive directive)
            (make-instance 'expression-statment :expression expression)))))))

(defun parse-directive-prologues (parser)
  (with-slots (lookahead context) parser
    (let ((first-restricted)
          (body))
      (loop
       (let ((token lookahead))
         (unless (typep token 'string-literal)
           (return))
         (let ((statement (parse-directive parser)))
           (appendf body (list statement))
           (let ((directive (slot-value 'statement 'directive)))
             (unless (typep directive 'string)
               (return))
             (cond
              ((equal "use strict" directive)
               (setf (getf context :strict) t)
               (when first-restricted
                 (tolerate-unexpected-token
                  parser first-restricted
                  "Octal literals are not allowed in strict mode"))
               (unless (getf context :allow-strict-directive)
                 (tolerate-unexpected-token
                  parser token
                  "Illegal \'use strict\' directive in function with non-simple parameter list")))
              ((and (not first-restricted) (slot-value token 'octal))
               (setf first-restricted token)))))))
      body)))

(defun qualified-property-name (token))

(defun parse-getter-method (parser))

(defun parse-setter-method (parser))

(defun parse-generator-method (parser))

(defun start-of-expression-p (parser))

(defun parse-yield-expression (parser))

(defun parse-class-element (parser))

(defun parse-class-element-list (parser))

(defun parse-class-body (parser))

(defun parse-class-declaration (parser &optional identifier-optional-p))

(defun parse-class-expression (parser))

(defun parse-module (parser)
  (with-slots (lookahead) parser
    (let ((body (parse-directive-prologues parser)))
      (loop while (not (typep lookahead 'eof))
            do (appendf body (list (parse-statement-list-item parser))))
      (make-instance 'module :body body))))

(defun parse-script (parser)
  (with-slots (lookahead) parser
    (let ((body (parse-directive-prologues parser)))
      (loop while (not (typep lookahead 'eof))
            do (appendf body (list (parse-statement-list-item parser))))
      (make-instance 'script :body body))))

(defun parse-module-specifier (parser))

;; import {<foo as bar>} ...;
(defun parse-import-specifier (parser)
  (with-slots (lookahead) parser
    (let ((imported)
          (local))
      (if (typep lookahead 'identifier)
          (progn
            (setf imported (parse-variable-identifier parser)
                  local imported)
            (when (match-contextual-keyword parser "as")
              (next-token parser)
              (setf local (parse-variable-identifier parser))))
        (progn
          (setf imported (parse-identifier-name parser)
                local imported)
          (if (match-contextual-keyword parser "as")
              (progn
                (next-token parser)
                (setf local (parse-variable-identifier parser)))
            (error "Unexpected token"))))
      (make-instance 'import-specifier :local local :imported imported))))

;; {foo, bar as bas}
(defun parse-named-imports (parser)
  (expect parser "{")
  (let ((specifiers))
    (loop while (not (match parser "}"))
          do (appendf specifiers (list (parse-import-specifier parser)))
          (unless (match parser "}")
            (expect parser ",")))
    (expect parser "}")
    specifiers))

;; import <foo> ...;
(defun parse-import-default-specifier (parser)
  (let ((local (parse-identifier-name parser)))
    (make-instance 'import-default-specifier :local local)))

;; import <* as foo> ...;
(defun parse-import-namespace-specifier (parser)
  (expect parser "*")
  (unless (match-contextual-keyword parser "as")
    (error "some error"))
  (next-token parser)
  (let ((local (parse-identifier-name parser)))
    (make-instance 'import-namespace-specifier :local local)))

(defun parser-import-declaration (parser)
  (with-slots (lookahead context) parser
    (when (getf context :in-function-body)
      (error "some error"))
    (expect-keyword parser "import")
    (let ((source)
          (specifiers))
      (if (typep lookahead 'string-literal)
          ;; import 'foo';
          (setf source (parse-module-specifier parser))
        (progn
          (cond
           ((match parser "{")
            ;; import {bar}
            (appendf specifiers (parse-named-imports parser)))
           ((match parser "*")
            ;; import * as foo
            (appendf specifiers (list (parse-import-namespace-specifier parser))))
           ((and (identifier-name-p lookahead)
                 (not (match-keyword parser "default")))
            ;; import foo
            (appendf specifiers (list (parse-import-namespace-specifier parser)))
            (when (match parser ",")
              (next-token parser)
              (cond
               ((match parser "*")
                ;; import foo, * as foo
                (appendf specifiers (list (parse-import-namespace-specifier parser))))
               ((match parser "{")
                ;; import foo, {bar}
                (appendf specifiers (parse-named-imports parser)))
               (t (error "Unexpected token")))))
           (t (next-token parser) (error "Unexpected token")))
          (unless (match-contextual-keyword parser "from")
            (error "some error"))
          (next-token parser)
          (setf source (parse-module-specifier parser))))
      (consume-semicolon parser)
      (make-instance 'import-declaration :specifiers specifiers :source source))))

(defun parse-export-specifier (parser)
  (let ((local (parse-identifier-name parser)))
    (let ((exported local))
      (when (match-contextual-keyword parser "as")
        (next-token parser)
        (setf exported (parse-identifier-name parser)))
      (make-instance 'export-specifier :local local :exported exported))))

(defun parser-export-declaration (parser)
  (with-slots (lookahead context) parser
    (when (getf context :in-function-body)
      (error "Illegal Export Declaration"))
    (expect-keyword parser)
    (let ((export-declaration))
      (cond
       ((match-keyword parser "default")
        ;; export default ...
        (next-token parser)
        (cond
         ((match-keyword parser "function")
          ;; export default function foo () {}
          ;; export default function () {}
          (let ((declaration (parse-function-declaration parser t)))
            (setf export-declaration (make-instance 'export-default-declaration
                                                    :declaration declaration))))
         ((match-keyword parser "class")
          ;; export default class foo {}
          (let ((declaration (parse-class-declaration parser t)))
            (setf export-declaration (make-instance 'export-default-declaration
                                                    :declaration declaration))))
         ((match-contextual-keyword parser "async")
          ;; export default async function foo () {}
          ;; export default async function () {}
          ;; export default async x => x
          (let ((declaration (if (match-async-function parser)
                                 (parse-function-declaration parser t)
                               (parse-assignment-expression parser))))
            (setf export-declaration (make-instance 'export-default-declaration
                                                    :declaration declaration))))
         (t (when (match-contextual-keyword parser "from")
              (error "Unexpected token"))
            ;; export default {};
            ;; export default [];
            ;; export default (1 + 2);
            (let ((declaration (if (match parser "{")
                                   (parse-object-initializer parser)
                                 (if (match parser "[")
                                     (parse-array-initializer parser)
                                   (parse-assignment-expression parser)))))
              (consume-semicolon parser)
              (setf export-declaration (make-instance 'export-default-declaration
                                                      :declaration declaration))))))
       ((match parser "*")
        ;; export * from 'foo';
        (next-token parser)
        (unless (match-contextual-keyword parser "from")
          (error "some error"))
        (next-token parser)
        (let ((source (parse-module-specifier parser)))
          (consume-semicolon parser)
          (setf export-declaration (make-instance 'export-all-declaration
                                                  :source source))))
       ((typep lookahead 'keyword)
        ;; export var foo = 1;
        (let ((declaration))
          (switch ((slot-value lookahead 'value) :test 'equal)
            ((or "let" "const")
             (setf declaration (parse-lexical-declaration parser)))
            ((or "var" "class" "function")
             (setf declaration (parse-statement-list-item parser)))
            (t (error "Unexpected token")))
          (setf export-declaration (make-instance 'export-named-declaration
                                                  :declaration declaration))))
       ((match-async-function parser)
        (let ((declaration (parse-function-declaration parser)))
          (setf export-declaration (make-instance 'export-named-declaration
                                                  :declaration declaration))))
       (t (let ((specifiers)
                (source)
                (export-from-identifier-p))
            (expect parser "{")
            (loop while (not (match parser "}"))
                  do (setf export-from-identifier-p
                           (or export-from-identifier-p
                               (match-keyword "default")))
                  (appendf specifiers (list (parse-export-specifier parser)))
                  (unless (match parser "}")
                    (expect parser ",")))
            (expect parser "}")
            (cond
             ((match-contextual-keyword parser "from")
              ;; export { default } from 'foo';
              ;; export { foo } from 'foo';
              (next-token parser)
              (setf source (parse-module-specifier parser))
              (consume-semicolon parser))
             (export-from-identifier-p
              (error "some error"))
             (t ;; export { foo };
                (consume-semicolon parser)))
            (setf export-declaration (make-instance 'export-named-declaration
                                                    :specifiers specifiers
                                                    :source source)))))
      export-declaration)))
