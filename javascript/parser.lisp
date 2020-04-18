(in-package :javascript)

(defun operator-precedence (operator)
  (switch (operator :test 'equal)
    (")" 0)
    (";" 0)
    ("," 0)
    ("=" 0)
    ("]" 0)
    ("||" 1)
    ("&&" 2)
    ("|" 3)
    ("^" 4)
    ("&" 5)
    ("==" 6)
    ("!=" 6)
    ("===" 6)
    ("!==" 6)
    ("<" 7)
    (">" 7)
    ("<=" 7)
    (">=" 7)
    ("<<" 8)
    (">>" 8)
    (">>>" 8)
    ("+" 9)
    ("-" 9)
    ("*" 11)
    ("/" 11)
    ("%" 11)))

(defstruct marker index line column)

(defclass parser ()
  ((scanner
    :initarg :scanner
    :initform nil)
   (lookahead
    :initform nil)
   (context
    :initform nil)
   (config
    :initarg :config
    :initform nil)
   (tokens
    :initform nil)
   (has-line-terminator-p
    :initform nil)
   (start-marker
    :initform nil)
   (last-marker
    :initform nil)))

(defmethod initialize-instance :after ((parser parser) &key)
  (with-slots (scanner context lookahead start-marker last-marker) parser
    (setf lookahead (make-instance 'eof
                                   :line-number (slot-value scanner 'line-number)
                                   :line-start 0
                                   :start 0
                                   :end 0))
    (setf context `(:allow-in t :allow-strict-directive t :allow-yield t)
          start-marker (make-marker :index 0
                                    :line (slot-value scanner 'line-number)
                                    :column 0)
          last-marker (make-marker :index 0
                                   :line (slot-value scanner 'line-number)
                                   :column 0))
    (next-token parser)
    (setf last-marker (make-marker
                       :index (slot-value scanner 'index)
                       :line (slot-value scanner 'line-number)
                       :column (- (slot-value scanner 'index)
                                  (slot-value scanner 'line-start))))))

(defgeneric parse (source)
  (:method ((source string))
   (let ((scanner (make-instance 'scanner :source source)))
     (let ((parser (make-instance 'parser
                                  :scanner scanner
                                  :config `(:range t :location t))))
       (parse-script parser))))
  (:method ((stream stream))
   (parse (alexandria::read-stream-content-into-string stream)))
  (:method ((pathname pathname))
   (parse (read-file-into-string pathname))))

(defun throw-error (message-format &rest values))

(defun tolerate-error (parser message-format &rest values))

(defun unexpected-token-error (token message))

(defun throw-unexpected-token (parser &optional token message))

(defun tolerate-unexpected-token (parser &optional token message))

(defun collect-comments (parser)
  (with-slots (scanner) parser
    (scan-comments scanner)))

(defun get-token-raw (parser token)
  (with-slots (scanner) parser
    (with-slots (source) scanner
      (with-slots (start end) token
        (subseq source start end)))))

(defun convert-token (token))

(defun next-token (parser)
  (with-slots (lookahead scanner context start-marker last-marker) parser
    (let ((token lookahead))
      (setf (marker-index last-marker) (slot-value scanner 'index)
            (marker-line last-marker) (slot-value scanner 'line-number)
            (marker-column last-marker) (- (slot-value scanner 'index)
                                           (slot-value scanner 'line-start)))
      (collect-comments parser)
      (unless (= (slot-value scanner 'index)
                 (marker-index start-marker))
        (setf (marker-index start-marker) (slot-value scanner 'index)
              (marker-line start-marker) (slot-value scanner 'line-number)
              (marker-column start-marker) (- (slot-value scanner 'index)
                                              (slot-value scanner 'line-start))))
      (let ((next-token (lex scanner)))
        (when (and next-token (getf context :strict) (typep next-token 'identifier))
          (when (strict-mode-reserved-word-p (slot-value next-token 'value))
            (change-class next-token 'keyword)))
        (setf lookahead next-token))
      token)))

(defun next-regex-token (parser))

(defun create-marker (parser)
  (with-slots (start-marker) parser
    (make-marker :index (marker-index start-marker)
                 :line (marker-line start-marker)
                 :column (marker-column start-marker))))

(defun start-marker (token &optional (last-line-start 0))
  (let ((column (- (slot-value token 'start)
                   (slot-value token 'line-start)))
        (line (slot-value token 'line-number)))
    (when (< column 0)
      (incf column last-line-start)
      (decf line))
    (make-marker :index (slot-value token 'start)
                 :line line
                 :column column)))

(defun finalize (parser marker node)
  (with-slots (config last-marker) parser
    (when (getf config :range)
      (setf (slot-value node 'range) `(,(marker-index marker)
                                       ,(marker-index last-marker))))
    (when (getf config :location)
      (let ((location (make-instance
                       'source-location
                       :start (make-instance 'position
                                             :line (marker-line marker)
                                             :column (marker-column marker))
                       :end (make-instance 'position
                                           :line (marker-line last-marker)
                                           :column (marker-column last-marker)))))
        (setf (slot-value node 'location) location)))
    node))

(defun expect (parser value)
  (let ((token (next-token parser)))
    (when (or (not (typep token 'punctuator))
              (not (equal value (slot-value token 'value))))
      (throw-unexpected-token parser token))))

(defun expect-comma-separator (parser)
  (with-slots (config lookahead) parser
    (if (getf config :tolerant)
        (let ((token lookahead))
          (cond
           ((and (typep token 'punctuator)
                 (equal "," (slot-value token 'value)))
            (next-token parser))
           ((and (typep token 'punctuator)
                 (equal ";" (slot-value token 'value)))
            (next-token parser)
            (tolerate-unexpected-token parser token))
           (t (tolerate-unexpected-token parser token "Unexpected token ~A"))))
      (expect parser ","))))

(defun expect-keyword (parser value)
  (let ((token (next-token parser)))
    (when (or (not (typep token 'keyword))
              (not (equal value (slot-value token 'name))))
      (throw-unexpected-token parser token))))

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
          (throw-unexpected-token (getf context :first-cover-initialized-name-error)))
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
  (with-slots (lookahead has-line-terminator-p) parser
    (cond
     ((match parser ";")
      (next-token parser))
     ((not has-line-terminator-p)
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

(defun parse-property-method (parser params)
  (with-slots (context) parser
    (setf (getf context :assignment-target-p) nil
          (getf context :binding-element-p) nil)
    (let ((previous-strict (getf context :strict))
          (previous-allow-strict-directive (getf context :allow-strict-directive)))
      (setf (getf context :allow-strict-directive) (getf params :simple))
      (let ((body (isolate-cover-grammar parser 'parse-function-source-elements)))
        (when (and (getf context :strict) (getf params :first-restricted))
          (tolerate-unexpected-token parser
                                     (getf :params :first-restricted)
                                     (getf :params :message)))
        (when (and (getf context :strict) (getf params :stricted))
          (tolerate-unexpected-token parser
                                     (getf :params :stricted)
                                     (getf :params :message)))
        (setf (getf context :strict) previous-strict
              (getf context :allow-strict-directive) previous-allow-strict-directive)
        body))))

(defun parse-property-method-function (parser)
  (with-slots (context) parser
    (let ((generator-p)
          (marker (create-marker parser))
          (previous-allow-yield (getf context :allow-yield)))
      (setf (getf context :allow-yield) t)
      (let* ((params (parse-format-parameters parser))
             (method (parse-property-method parser params)))
        (setf (getf context :allow-yield) previous-allow-yield)
        (finalize parser marker (make-instance 'function-expression
                                               :id nil
                                               :params params
                                               :body method
                                               :generator generator-p))))))

(defun parse-property-method-async-function (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (previous-allow-yield (getf context :allow-yield))
          (previous-await (getf context :await)))
      (setf (getf context :allow-yield) t
            (getf context :await) t)
      (let* ((params (parse-formal-parameters parser))
             (method (parse-property-method parser params)))
        (setf (getf context :allow-yield) previous-allow-yield
              (getf context :await) previous-await)
        (finalize parser marker (make-instance 'async-function-expression
                                               :id nil
                                               :params params
                                               :body method))))))

(defun parse-object-property-key (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (token (next-token parser))
          (key))
      (typecase token
        ((string-literal numeric-literal)
         (when (and (getf context :strict)
                    (slot-value token 'octal))
           (tolerate-unexpected-token parser token "some message")
           (let ((raw (get-token-raw parser token)))
             (setf key (finalize parser marker (make-instance 'literal
                                                              :value (slot-value token 'value)))))))
        ((identifier boolean-literal null-literal keyword)
         (setf key (finalize parser marker (make-instance 'identifier
                                                          :name (slot-value token 'value)))))
        (punctuator
         (if (equal "[" (slot-value token 'value))
             (progn
               (setf key (isolate-cover-grammar parser 'parse-assignment-expression))
               (expect parser "]"))
           (setf key (throw-unexpected-token parser token))))
        (t (setf key (throw-unexpected-token parser token))))
      key)))

(defun property-key-p (key value)
  (or (and (typep key 'identifier)
           (equal value (slot-value key 'name)))
      (and (typep key 'literal)
           (equal value (slot-value key 'value)))))

(defun parse-object-property (parser has-proto)
  (with-slots (lookahead context has-line-terminator-p) parser
    (let ((marker (create-marker parser))
          (token lookahead)
          (kind)
          (key)
          (value)
          (computed)
          (method)
          (shorthand)
          (async-p))
      (cond
       ((typep token 'identifier)
        (let ((id (slot-value token 'value)))
          (next-token parser)
          (setf computed (match parser "[")
                async-p (and (not has-line-terminator-p)
                             (equal "async" id)
                             (not (match parser ":"))
                             (not (match parser "("))
                             (not (match parser "*"))
                             (not (match parser ",")))
                key (if async-p
                        (parse-object-property-key parser)
                      (finalize parser marker (make-instance 'identifier :name id))))))
       ((match parser "*")
        (next-token parser))
       (t (setf computed (match parser "[")
                key (parse-object-property-key parser))))
      (let ((lookahead-property-key (qualified-property-name lookahead)))
        (cond
         ((and (typep token 'identifier)
               (not async-p)
               (equal "get" (slot-value token 'value))
               lookahead-property-key)
          (setf kind "get"
                computed (match parser "[")
                key (parse-object-property-key parser)
                (getf context :allow-yield) nil
                value (parse-getter-method parser)))
         ((and (typep token 'identifier)
               (not async-p)
               (equal "set" (slot-value token 'value))
               lookahead-property-key)
          (setf kind "set"
                computed (match parser "[")
                key (parse-object-property-key parser)
                value (parse-setter-method parser)))
         ((and (typep token 'punctuator)
               (equal "*" (slot-value token 'value))
               lookahead-property-key)
          (setf kind "init"
                computed (match parser "[")
                key (parse-object-property-key parser)
                value (parse-setter-method parser)
                method t))
         (t (unless key
              (throw-unexpected-token parser lookahead))
            (setf kind "init")
            (cond
             ((and (match parser ":") (not async-p))
              (when (and (not computed) (property-key-p key "__proto__"))
                (when (slot-value has-proto 'value)
                  (tolerate-error parser "some message"))
                (setf (slot-value has-proto 'value) t))
              (next-token parser)
              (setf value (inherit-cover-grammar parser 'parse-assignment-expression)))
             ((match parser "(")
              (setf value (if async-p
                              (parse-property-method-async-function parser)
                            (parse-property-method-function parser))
                    method t))
             ((typep token 'identifier)
              (let ((id (finalize parser marker (make-instance 'identifier
                                                               :name (slot-value token 'value)))))
                (if (match parser "=")
                    (progn
                      (setf (getf context :first-cover-initialized-name-error) lookahead)
                      (next-token parser)
                      (setf shorthand t)
                      (let ((init (isolate-cover-grammar parser 'parse-assignment-expression)))
                        (setf value (finalize parser marker (make-instance 'assignment-pattern
                                                                           :left id
                                                                           :right init)))))
                  (setf shorthand t
                        value id))))
             (t (throw-unexpected-token parser (next-token parser)))))))
      (finalize parser marker (make-instance 'property
                                             :kind kind
                                             :key key
                                             :computed computed
                                             :value value
                                             :method method
                                             :shorthand shorthand)))))

(defun parse-object-initializer (parser)
  (let ((marker (create-marker parser)))
    (expect parser "{")
    (let ((properties)
          (has-proto `(:value nil)))
      (loop while (not (match parser "}"))
            do (appendf properties (list (parse-object-property parser has-proto)))
            (unless (match parser "}")
              (expect-comma-separator parser)))
      (expect parser "}")
      (finalize parser marker (make-instance 'object-expression :properties properties)))))

(defun parse-template-head (parser))

(defun parse-template-element (parser))

(defun parse-template-literal (parser))

(defun reinterpret-expression-as-pattern (expression)
  (typecase expression
    ((identifier member-expression rest-element assignment-pattern))
    (spread-element
     (change-class expression 'rest-element)
     (reinterpret-expression-as-pattern (slot-value expression 'argument)))
    (array-expression
     (change-class expression 'array-pattern)
     (loop for i from 0 upto (1- (length (slot-value expression 'elements)))
           when (nth i (slot-value expression 'elements))
           do (reinterpret-expression-as-pattern
               (nth i (slot-value expression 'elements)))))
    (object-expression
     (change-class expression 'object-pattern)
     (loop for i from 0 upto (1- (length (slot-value expression 'properties)))
           do (reinterpret-expression-as-pattern
               (nth i (slot-value expression 'elements)))))
    (assignment-expression
     (change-class expression 'assignment-pattern)
     (setf (slot-value expression 'operator) nil)
     (reinterpret-expression-as-pattern (slot-value expression 'left)))))

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
        ((match parser "(")
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
                     do (reinterpret-expression-as-pattern (nth arguments i)))
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
           (setf statement (parse-export-declaration parser)))
          ("import"
           (unless (getf context :module-p)
             (tolerate-unexpected-token parser lookahead "Unexpected token"))
           (setf statement (parse-import-declaration parser)))
          ("const"
           (setf statement (parse-lexical-declaration parser)))
          ("function"
           (setf statement (parse-function-declaration parser)))
          ("class"
           (setf statement (parse-class-declaration parser)))
          ("let"
           (setf statement (if (lexical-declaration-p parser)
                               (parse-lexical-declaration parser)
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

(defun parse-statement-list-item (parser)
  (with-slots (lookahead context) parser
    (let ((statement))
      (setf (getf context :assignment-target-p) t
            (getf context :binding-element-p) t)
      (if (typep lookahead 'keyword)
          (switch ((slot-value lookahead 'name) :test 'equal)
            ("export"
             (unless (getf context :module-p)
               (tolerate-unexpected-token parser lookahead "some message"))
             (setf statement (parse-export-declaration parser)))
            ("import"
             (unless (getf context :module-p)
               (tolerate-unexpected-token parser lookahead "some message"))
             (setf statement (parse-import-declaration parser)))
            ("const" (setf statement (parse-lexical-declaration parser)))
            ("function" (setf statement (parse-function-declaration parser)))
            ("class" (setf statement (parse-class-declaration parser)))
            ("let" (setf statement (if (lexical-declaration-p parser)
                                       (parse-lexical-declaration parser)
                                     (parse-statement parser))))
            (t (setf statement (parse-statement parser))))
        (setf statement (parse-statement parser)))
      statement)))

(defun parse-block (parser))

(defun parse-lexical-binding (parser kind options))

(defun parse-binding-list (parser kind options))

(defun lexical-declaration-p (parser))

(defun parse-lexical-declaration (parser &key in-for))

(defun parse-binding-rest-element (parser params kind))

(defun parse-array-pattern (parser params kind))

(defun parse-property-pattern (parser params kind))

(defun parse-object-pattern (parser params kind))

(defun parse-pattern (parser params &optional kind)
  (with-slots (lookahead) parser
    (let ((pattern))
      (cond
       ((match parser "[")
        (setf pattern (parse-array-pattern parser params kind)))
       ((match parser "{")
        (setf pattern (parse-object-pattern parser params kind)))
       (t (when (and (match-keyword parser "let")
                     (or (equal "const" kind)
                         (equal "let" kind)))
            (tolerate-unexpected-token parser lookahead "some message"))
          (appendf params (list lookahead))
          (setf pattern (parse-variable-identifier parser kind))))
      pattern)))

(defun parse-pattern-with-default (parser params &optional kind)
  (with-slots (lookahead context) parser
    (let ((start-token lookahead)
          (pattern (parse-pattern parser params kind)))
      (when (match parser "=")
        (next-token parser)
        (let ((previous-allow-yield (getf context :allow-yield)))
          (setf (getf context :allow-yield) t)
          (let ((right (isolate-cover-grammar parser 'parse-assignment-expression)))
            (setf (getf context :allow-yield) previous-allow-yield
                  pattern (finalize parser (start-marker parser start-token)
                                    (make-instance 'assignment-pattern
                                                   :left pattern
                                                   :right right)))))))))

(defun parse-variable-identifier (parser &optional kind)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (token (next-token parser)))
      (cond
       ((and (typep token 'keyword)
             (equal "yield" (slot-value token 'name)))
        (cond
         ((getf context :strict)
          (tolerate-unexpected-token parser token "some message"))
         ((not (getf context :allow-yield))
          (throw-unexpected-token parser token))))
       ((not (typep token 'identifier))
        (if (and (getf context :strict)
                 (typep token 'keyword)
                 (strict-mode-reserved-word-p (slot-value token 'name)))
            (tolerate-unexpected-token parser token "some message")
          (when (or (getf context :strict)
                    (not (equal "let" (slot-value token 'value)))
                    (not (equal "var" kind)))
            (throw-unexpected-token parser token))))
       ((and (or (getf context :module-p)
                 (getf context :await))
             (typep token 'identifier)
             (equal "await" (slot-value token 'name)))
        (tolerate-unexpected-token parser token)))
      (finalize parser marker (make-instance 'identifier
                                             :name (slot-value token 'name))))))

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

(defun parse-switch-case (parser))

(defun parse-switch-statement (parser))

(defun parse-labelled-statement (parser))

(defun parse-throw-statement (parser))

(defun parse-catch-statement (parser))

(defun parse-finally-clause (parser))

(defun parse-try-statement (parser))

(defun parse-debugger-statement (parser))

(defun parse-statement (parser))

(defun parse-function-source-elements (parser)
  (with-slots (lookahead context) parser
    (let ((marker (create-marker parser)))
      (expect parser "{")
      (let ((body (parse-directive-prologues parser))
            (previous-label-set (getf context :label-set))
            (previous-in-iteration (getf context :in-iteration))
            (previous-in-switch (getf context :in-switch))
            (previous-in-function-body (getf context :in-function-body)))
        (setf (getf context :label-set) nil
              (getf context :in-iteration) nil
              (getf context :in-switch) nil
              (getf context :in-function-body) t)
        (loop while (not (typep lookahead 'eof))
              do (when (match parser "}")
                   (return))
              (appendf body (list (parse-statement-list-item parser))))
        (expect parser "}")
        (setf (getf context :label-set) previous-label-set
              (getf context :in-iteration) previous-in-iteration
              (getf context :in-switch) previous-in-switch
              (getf context :in-function-body) previous-in-function-body)
        (finalize parser marker (make-instance 'block-statement :body body))))))

(defun validate-param (parser options param name)
  (with-slots (context) parser
    (let ((key (format nil "$~A" name)))
      (cond
       ((getf context :strict)
        (when (restricted-word-p name)
          (setf (getf options :stricted) param
                (getf options :message) "some message"))
        (when (getf (getf options :param-set) key)
          (setf (getf options :stricted) param
                (getf options :message) "some message")))
       ((not (getf options :first-restricted))
        (cond
         ((restricted-word-p name)
          (setf (getf options :first-restricted) param
                (getf options :message) "some message"))
         ((strict-mode-reserved-word-p name)
          (setf (getf options :first-restricted) param
                (getf options :message) "some message"))
         ((getf (getf options :param-set) key)
          (setf (getf options :first-restricted) param
                (getf options :message) "some message")))))
      (setf (getf (getf options :param-set) key) t))))

(defun parse-rest-element (parser params)
  (let ((marker (create-marker parser)))
    (expect parser "...")
    (let ((argument (parse-pattern parser params)))
      (when (match parser "=")
        (throw-error parser "some message"))
      (unless (match parser ")")
        (throw-error parser "some message"))
      (finalize parser marker (make-instance 'rest-element :argument argument)))))

(defun parse-formal-parameter (parser options)
  (let* ((params)
         (param (if (match parser "...")
                    (parse-rest-element parser params)
                  (parse-pattern-with-default parser params))))
    (loop for param in params
          do (validate-param parser options param (slot-value param 'value)))
    (setf (getf options :simple) (and (getf options :simple)
                                      (typep param 'identifier)))
    (appendf (getf options :params) (list param))
    param))

(defun parse-formal-parameters (parser &optional first-restricted)
  (with-slots (lookahead) parser
    (let ((options `(:simple t :params nil :first-restricted ,first-restricted)))
      (expect parser "(")
      (unless (match parser ")")
        (setf (getf options :param-set) nil)
        (loop while (not (typep lookahead 'eof))
              do (parse-formal-parameter parser options)
              (when (match parser ")")
                (return))
              (expect parser ",")
              (when (match parser ")")
                (return))))
      (expect parser ")")
      `(:simple ,(getf options :simple)
        :params ,(getf options :params)
        :stricted ,(getf options :stricted)
        :first-restricted ,(getf options :first-restricted)
        :message ,(getf options :message)))))

(defun match-async-function (parser))

(defun parse-function-declaration (parser &optional identifier-optional-p)
  (with-slots (lookahead context) parser
    (let ((marker (create-marker parser))
          (async-p (match-contextual-keyword parser "async")))
      (when async-p (next-token parser))
      (expect-keyword parser "function")
      (let ((generator-p (if async-p nil (match parser "*"))))
        (when generator-p (next-token parser))
        (let ((message)
              (id)
              (first-restricted))
          (when (or (not identifier-optional-p)
                    (not (match parser "(")))
            (let ((token lookahead))
              (setf id (parse-variable-identifier parser))
              (if (getf context :strict)
                  (cond
                   ((restricted-word-p (slot-value token 'value))
                    (setf first-restricted token
                          message "some message"))
                   ((strict-mode-reserved-word-p (slot-value token 'value))
                    (setf first-restricted token
                          message "some message"))))))
          (let ((previous-allow-await (getf context :await))
                (previous-allow-yield (getf context :allow-yield)))
            (setf (getf context :await) async-p
                  (getf context :allow-yield) (not generator-p))
            (let* ((formal-parameters (parse-formal-parameters parser first-restricted))
                   (params (getf formal-parameters :params))
                   (stricted (getf formal-parameters :stricted))
                   (first-restricted (getf formal-parameters :first-restricted)))
              (when (getf formal-parameters :message)
                (setf message (getf formal-parameters :message)))
              (let ((previous-strict (getf context :strict))
                    (previous-allow-strict-directive (getf context :allow-strict-directive)))
                (setf (getf context :allow-strict-directive) (getf formal-parameters 'simple))
                (let ((body (parse-function-source-elements parser)))
                  (when (and (getf context :strict) first-restricted)
                    (throw-unexpected-token parser first-restricted message))
                  (when (and (getf context :strict) stricted)
                    (throw-unexpected-token parser stricted message))
                  (setf (getf context :strict) previous-strict
                        (getf context :allow-strict-directive) previous-allow-strict-directive
                        (getf context :await) previous-allow-await
                        (getf context :allow-yield) previous-allow-yield)
                  (if async-p
                      (finalize parser marker (make-instance 'async-function-declaration
                                                             :id id
                                                             :params params
                                                             :body body
                                                             :generator generator-p))
                    (finalize parser marker (make-instance 'function-declaration
                                                           :id id
                                                           :params params
                                                           :body body
                                                           :generator generator-p))))))))))))

(defun parse-function-expression (parser))

(defun parse-directive (parser)
  (with-slots (lookahead) parser
    (let ((token lookahead)
          (marker (create-marker parser)))
      (let ((expression (parse-expression parser)))
        (let ((directive (when (typep expression 'literal)
                           (let ((raw (get-token-raw parser token)))
                             (subseq raw 1 (1- (length raw)))))))
          (consume-semicolon parser)
          (finalize parser marker
                    (if directive
                        (make-instance 'directive :expression expression :directive directive)
                      (make-instance 'expression-statment :expression expression))))))))

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
      (loop while (and lookahead (not (typep lookahead 'eof)))
            do (appendf body (list (parse-statement-list-item parser))))
      (make-instance 'module :body body))))

(defun parse-script (parser)
  (with-slots (lookahead) parser
    (let ((body (parse-directive-prologues parser)))
      (loop while (and lookahead (not (typep lookahead 'eof)))
            for statement = (parse-statement-list-item parser)
            while statement
            do (appendf body (list statement)))
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

(defun parse-import-declaration (parser)
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

(defun parse-export-declaration (parser)
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

(defun trace-parser ()
  (trace parse-primary-expression
         parse-spread-element
         parse-array-initializer
         parse-property-method
         parse-property-method-function
         parse-property-method-async-function
         parse-object-property-key
         parse-template-head
         parse-template-element
         parse-template-literal
         parse-group-expression
         parse-arguments
         parse-identifier-name
         parse-new-expression
         parse-async-argument
         parse-async-arguments
         parse-left-hand-side-expression-allow-call
         parse-left-hand-side-expression
         parse-update-expression
         parse-await-expression
         parse-unary-expression
         parse-exponentiation-expression
         parse-binary-expression
         parse-conditional-expression
         parse-assignment-expression
         parse-expression
         parse-statement-list-item
         parse-block
         parse-lexical-binding
         parse-binding-list
         parse-lexical-declaration
         parse-binding-rest-element
         parse-array-pattern
         parse-property-pattern
         parse-object-pattern
         parse-pattern
         parse-pattern-with-default
         parse-variable-identifier
         parse-variable-declaration
         parse-variable-declaration-list
         parse-variable-statement
         parse-empty-statement
         parse-expression-statement
         parse-if-clause
         parse-if-statement
         parse-do-while-statement
         parse-while-statement
         parse-for-statement
         parse-continue-statement
         parse-break-statement
         parse-return-statement
         parse-with-statement
         parse-switch-case
         parse-switch-statement
         parse-labelled-statement
         parse-throw-statement
         parse-catch-statement
         parse-finally-clause
         parse-try-statement
         parse-debugger-statement
         parse-statement
         parse-function-source-elements
         parse-rest-element
         parse-formal-parameter
         parse-formal-parameters
         parse-function-declaration
         parse-function-expression
         parse-directive
         parse-directive-prologues
         parse-getter-method
         parse-setter-method
         parse-generator-method
         parse-yield-expression
         parse-class-element
         parse-class-element-list
         parse-class-body
         parse-class-declaration
         parse-class-expression
         parse-module
         parse-script
         parse-module-specifier
         parse-import-specifier
         parse-named-imports
         parse-import-default-specifier
         parse-import-namespace-specifier
         parse-import-declaration
         parse-export-specifier
         parse-export-specifier
         parse-export-declaration))