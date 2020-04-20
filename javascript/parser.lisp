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

(defun throw-error (message-format &rest values)
  (apply 'error message-format values))

(defun tolerate-error (parser message-format &rest values))

(defun unexpected-token-error (token message))

(defun throw-unexpected-token (parser &optional token message)
  (declare (ignore parser))
  (unexpected-token-error token message))

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

(defun next-regex-token (parser)
  (with-slots (scanner lookahead) parser
    (collect-comments parser)
    (let ((token (scan-reg-exp scanner)))
      (setf lookahead token)
      (next-token parser)
      token)))

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
           (t (tolerate-unexpected-token parser token "some message"))))
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
         (equal value (slot-value lookahead 'name)))))

(defun match-contextual-keyword (parser value)
  (with-slots (lookahead) parser
    (and (typep lookahead 'identifier)
         (equal value (slot-value lookahead 'name)))))

(defun match-assign (parser)
  (with-slots (lookahead) parser
    (unless (typep lookahead 'punctuator)
      (return-from match-assign))
    (let ((operator (slot-value lookahead 'value)))
      (member operator '("=" "*=" "**=" "/=" "%=" "+=" "-="
                         "<<=" ">>=" ">>>=" "&=" "^=" "|=")
              :test 'equal))))

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
    (let ((marker (create-marker parser))
          (expression)
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
                            (finalize parser marker
                                      (make-instance 'identifier
                                                     :name (slot-value (next-token parser) 'name))))))
        ((or numeric-literal string-literal)
         (when (and (getf context :strict)
                    (slot-value lookahead 'octal))
           (tolerate-unexpected-token parser lookahead "some message"))
         (setf (getf context :assignment-target-p) nil
               (getf context :binding-elemnt-p) nil
               token (next-token parser)
               raw (get-token-raw parser token)
               expression (finalize parser marker
                                    (make-instance 'literal :value (slot-value token 'value)))))
        (boolean-literal
         (setf (getf context :assignment-target-p) nil
               (getf context :binding-elemnt-p) nil
               token (next-token parser)
               raw (get-token-raw parser token)
               expression (finalize parser marker
                                    (make-instance 'literal
                                                   :value (equal "true" (slot-value token 'value))))))
        (null-literal
         (setf (getf context :assignment-target-p) nil
               (getf context :binding-elemnt-p) nil
               token (next-token parser)
               raw (get-token-raw parser token)
               expression (finalize parser marker (make-instance 'literal))))
        (template-literal
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
                  expression (finalize parser marker (make-instance 'reg-exp-literal))))
           (t (throw-unexpected-token parser (next-token parser)))))
        (keyword
         (cond
          ((and (not (getf context :strict))
                (getf context :allow-yield)
                (match-keyword parser "yield"))
           (setf expression (parse-identifier-name parser)))
          ((and (not (getf context :strict))
                (match-keyword parser "let"))
           (setf expression (finalize parser marker
                                      (make-instance 'identifier
                                                     :name (slot-value (next-token parser) 'value)))))
          (t (setf (getf context :assignment-target-p) nil
                   (getf context :binding-elemnt-p) nil)
             (cond
              ((match-keyword parser "function")
               (setf expression (parse-function-expression parser)))
              ((match-keyword parser "this")
               (next-token parser)
               (setf expression (finalize parser marker
                                          (make-instance 'this-expression))))
              ((match-keyword parser "class")
               (setf expression (parse-class-expression parser)))
              (t (throw-unexpected-token parser (next-token parser)))))))
        (t (throw-unexpected-token parser (next-token parser))))
      expression)))

(defun parse-spread-element (parser)
  (let ((marker (create-marker parser)))
    (expect parser "...")
    (let ((argument (inherit-cover-grammar parser 'parse-assignment-expression)))
      (finalize parser marker (make-instance 'spread-element :argument argument)))))

(defun parse-array-initializer (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (elements))
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
                (t (appendf elements (list (inherit-cover-grammar
                                            parser
                                            'parse-assignment-expression)))
                   (unless (match parser "]")
                     (expect parser ",")))))
      (expect parser "]")
      (finalize parser marker (make-instance 'array-expression :elements elements)))))

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
      (let* ((params (parse-formal-parameters parser))
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
        ((or string-literal numeric-literal)
         (when (and (getf context :strict)
                    (slot-value token 'octal))
           (tolerate-unexpected-token parser token "some message")
           (let ((raw (get-token-raw parser token)))
             (setf key (finalize parser marker (make-instance 'literal
                                                              :value (slot-value token 'value)))))))
        ((or identifier boolean-literal null-literal keyword)
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
        (let ((id (slot-value token 'name)))
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
      (let ((lookahead-property-key (qualified-property-name-p lookahead)))
        (cond
         ((and (typep token 'identifier)
               (not async-p)
               (equal "get" (slot-value token 'name))
               lookahead-property-key)
          (setf kind "get"
                computed (match parser "[")
                key (parse-object-property-key parser)
                (getf context :allow-yield) nil
                value (parse-getter-method parser)))
         ((and (typep token 'identifier)
               (not async-p)
               (equal "set" (slot-value token 'name))
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

(defun parse-template-head (parser)
  (let* ((marker (create-marker parser))
         (token (next-token parser))
         (raw (slot-value token 'value))
         (cooked (slot-value token 'cooked)))
    (finalize parser marker (make-instance 'template-element
                                           :tail (slot-value token 'tail)
                                           :value-cooked cooked
                                           :value-raw raw))))

(defun parse-template-element (parser)
  (with-slots (lookahead) parser
    (unless (typep lookahead 'template-literal)
      (throw-unexpected-token parser))
    (let* ((marker (create-marker parser))
           (token (next-token parser))
           (raw (slot-value token 'raw))
           (cooked (slot-value token 'cooked)))
      (finalize parser marker (make-instance 'template-element
                                             :tail (slot-value token 'tail)
                                             :value-cooked cooked
                                             :value-raw raw)))))

(defun parse-template-literal (parser)
  (let ((marker (create-marker parser))
        (expressions)
        (quasis))
    (let ((quasi (parse-template-head parser)))
      (appendf quasis (list quasi))
      (loop while (not (slot-value quasi 'tail))
            do (appendf expressions (list (parse-expression parser)))
            (setf quasi (parse-template-element parser))
            (appendf quasis (list quasi)))
      (finalize parser marker (make-instance 'template-literal
                                             :quasis quasis
                                             :expressions expressions)))))

(defun reinterpret-expression-as-pattern (expression)
  (typecase expression
    ((or identifier member-expression rest-element assignment-pattern))
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

(defun parse-group-expression (parser)
  (with-slots (context lookahead) parser
    (let ((expression))
      (expect parser "(")
      (if (match parser ")")
          (progn
            (next-token parser)
            (unless (match parser "=>")
              (expect parser "=>"))
            (setf expression `(:type :arrow-parameter-placeholder
                               :params nil
                               :async nil)))
        (let ((start-token lookahead)
              (params))
          (if (match parser "...")
              (progn
                (setf expression (parse-rest-element parser params))
                (expect parser ")")
                (unless (match parser "=>")
                  (expect parser "=>"))
                (setf expression `(:type :arrow-parameter-placeholder
                                   :params `(,expression)
                                   :async nil)))
            (let ((arrow))
              (setf (getf context :binding-element-p) t
                    expression (inherit-cover-grammar parser 'parse-assignment-expression))
              (when (match parser ",")
                (let ((expressions))
                  (setf (getf context :assignment-target-p) nil)
                  (appendf expressions (list expression))
                  (loop while (not (typep lookahead 'eof))
                        do (unless (match parser ",")
                             (return))
                        (next-token parser)
                        (cond
                         ((match parser ")")
                          (next-token parser)
                          (loop for expression in expressions
                                do (reinterpret-expression-as-pattern expression))
                          (setf arrow t
                                expression `(:type :arrow-parameter-placeholder
                                             :params expressions
                                             :async nil)))
                         ((match parser "...")
                          (unless (getf context :binding-element-p)
                            (throw-unexpected-token parser lookahead))
                          (appendf expressions (list (parse-rest-element parser params)))
                          (expect parser ")")
                          (unless (match parser "=>")
                            (expect parser "=>"))
                          (setf (getf context :binding-element-p) nil)
                          (loop for expression in expressions
                                do (reinterpret-expression-as-pattern expression))
                          (setf arrow t
                                expression `(:type :arrow-parameter-placeholder
                                             :params expressions
                                             :async nil)))
                         (t (appendf expressions (list (inherit-cover-grammar parser
                                                                              'parse-assignment-expression)))))
                        (when arrow (return)))
                  (unless arrow
                    (setf expression (finalize parser (start-marker start-token)
                                               (make-instance 'sequence-expression
                                                              :expressions expressions))))))
              (unless arrow
                (expect parser ")")
                (when (match parser "=>")
                  (when (and (typep expression 'identifier)
                             (equal "yield" (slot-value expression 'name)))
                    (setf arrow t
                          expression `(:type :arrow-parameter-placeholder
                                       :params `(,expression)
                                       :async nil)))
                  (unless arrow
                    (unless (getf context :binding-element-p)
                      (throw-unexpected-token parser lookahead))
                    (if (typep expression 'sequence-expression)
                        (loop for expression in (slot-value expression 'expressions)
                              do (reinterpret-expression-as-pattern expression))
                      (reinterpret-expression-as-pattern expression))
                    (let ((parameters (if (typep expression 'sequence-expression)
                                          (slot-value expression 'expressions)
                                        `(,expression))))
                      (setf expression `(:type :arrow-parameter-placeholder
                                         :params ,parameters
                                         :async nil)))))
                (setf (getf context :binding-element-p) nil))))))
      expression)))

(defun parse-arguments (parser)
  (expect parser "(")
  (let ((arguments))
    (unless (match parser ")")
      (loop
       (let ((expression (if (match parser "...")
                             (parse-spread-element parser)
                           (isolate-cover-grammar parser 'parse-assignment-expression))))
         (appendf arguments (list expression))
         (when (match parser ")")
           (return))
         (expect-comma-separator parser)
         (when (match parser ")")
           (return)))))
    (expect parser ")")
    arguments))

(defun identifier-name-p (token)
  (typecase token
    ((or identifier keyword boolean-literal null-literal) t)))

(defun parse-identifier-name (parser)
  (let ((marker (create-marker parser))
        (token (next-token parser)))
    (unless (identifier-name-p token)
      (throw-unexpected-token token))
    (finalize parser marker (make-instance 'identifier
                                           :name (slot-value token 'name)))))

(defun parse-new-expression (parser)
  (with-slots (lookahead context) parser
    (let ((marker (create-marker parser))
          (id (parse-identifier-name parser)))
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
                (throw-unexpected-token parser lookahead)))
          (let ((callee (isolate-cover-grammar parser 'parse-left-hand-side-expression))
                (arguments (when (match parser "(")
                             (parse-arguments parser))))
            (setf expression (make-instance 'new-expression
                                            :callee callee
                                            :arguments arguments)
                  (getf context :assignment-target-p) nil
                  (getf context :binding-element-p) nil)))
        (finalize parser marker expression)))))

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
            (setf expression (finalize parser expression (make-instance 'super)))
            (when (and (not (match parser "("))
                       (not (match parser "."))
                       (not (match parser "[")))
              (throw-unexpected-token parser lookahead)))
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
           (setf expression (finalize parser (start-marker start-token)
                                      (make-instance 'static-member-expression
                                                     :object expression
                                                     :property property)))))
        ((match parser "(")
         (let ((async-arrow (and maybe-async)))
           (setf (getf context :binding-element-p) nil
                 (getf context :assignment-target-p) nil)
           (let ((arguments (if async-arrow
                                (parse-async-arguments parser)
                              (parse-arguments parser))))
             (setf expression (finalize parser (start-marker start-token)
                                        (make-instance 'call-expression
                                                       :callee expression
                                                       :arguments arguments)))
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
           (setf expression (finalize parser (start-marker start-token)
                                      (make-instance 'computed-member-expression
                                                     :object expression
                                                     :property property)))))
        ((and (typep lookahead 'template-literal)
              (slot-value lookahead 'head))
         (let ((quasi (parse-template-literal parser)))
           (setf expression (finalize parser (start-marker start-token)
                                      (make-instance 'tagged-template-expression
                                                     :tag expression
                                                     :quasi quasi)))))
        (t (return))))
      (setf (getf context :allow-in) previous-allow-in)
      expression)))

(defun parse-super (parser)
  (with-slots (lookahead) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "super")
      (when (and (not (match parser "["))
                 (not (match parser ".")))
        (throw-unexpected-token parser lookahead))
      (finalize parser marker (make-instance 'super)))))

(defun parse-left-hand-side-expression (parser)
  (with-slots (lookahead context) parser
    (let ((marker (start-marker lookahead))
          (expression (if (and (match parser "super")
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
           (setf expression (finalize parser marker
                                      (make-instance 'computed-mumber-expression
                                                     :object expression
                                                     :property property)))))
        ((match parser ".")
         (setf (getf context :binding-element-p) nil
               (getf context :assignment-target-p) t)
         (expect parser ".")
         (let ((property (parse-identifier-name parser)))
           (expect parser "]")
           (setf expression (finalize parser marker
                                      (make-instance 'static-member-expression
                                                     :object expression
                                                     :property property)))))
        ((and (typep lookahead 'template-literal)
              (slot-value lookahead 'head))
         (let ((quasi (parse-template-literal parser)))
           (setf expression (finalize parser marker
                                      (make-instance 'tagged-template-expression
                                                     :tag expression
                                                     :quasi quasi)))))
        (t (return))))
      expression)))

(defun parse-update-expression (parser)
  (with-slots (lookahead context) parser
    (let ((expression)
          (start-token lookahead))
      (if (or (match parser "++")
              (match parser "--"))
          (let ((marker (start-marker start-token))
                (token (next-token parser)))
            (setf expression (inherit-cover-grammar parser 'parser-unary-expression))
            (when (and (getf context :strict)
                       (typep expression 'identifier)
                       (restricted-word-p (slot-value expression 'name)))
              (tolerate-error parser "some message"))
            (unless (getf context :assignment-target-p)
              (tolerate-error parser "some message"))
            (let ((prefix t))
              (setf expression (finalize parser marker
                                         (make-instance 'update-expression
                                                        :operator (slot-value token 'value)
                                                        :argument expression
                                                        :prefix prefix))
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
                (tolerate-error parser "some message"))
              (unless (getf context :assignment-target-p)
                (tolerate-error parser "some message"))
              (setf (getf context :assignment-target-p) nil
                    (getf context :binding-element-p) nil)
              (let ((operator (slot-value (next-token parser) 'value))
                    (prefix nil))
                (setf expression (finalize parser (start-marker start-token)
                                           (make-instance 'update-expression
                                                          :operator operator
                                                          :argument expression
                                                          :prefix prefix))))))))
      expression)))

(defun parse-await-expression (parser)
  (let ((marker (create-marker parser)))
    (next-token parser)
    (let ((argument (parse-unary-expression parser)))
      (finalize parser marker
                (make-instance 'await-expression :argument argument)))))

(defun parse-unary-expression (parser)
  (with-slots (context lookahead) parser
    (let ((expression))
      (cond
       ((or (match parser "+")
            (match parser "-")
            (match parser "~")
            (match parser "!")
            (match parser "delete")
            (match parser "void")
            (match parser "typeof"))
        (let ((marker (start-marker lookahead))
              (token (next-token parser)))
          (setf expression (inherit-cover-grammar parser 'parse-unary-expression)
                expression (finalize parser marker
                                     (make-instance 'unary-expression
                                                    :operator (slot-value token 'value)
                                                    :argument expression)))
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
  (with-slots (context lookahead) parser
    (let ((start-token lookahead)
          (expression (inherit-cover-grammar parser 'parse-unary-expression)))
      (when (and (not (typep expression 'unary-expression))
                 (match parser "**"))
        (next-token parser)
        (setf (getf context :assignment-target-p) nil
              (getf context :binding-element-p) nil)
        (let ((left expression)
              (right (isolate-cover-grammar parser 'parse-exponentiation-expression)))
          (setf expression (finalize parser (start-marker start-token)
                                     (make-instance 'binary-expression
                                                    :operator "**"
                                                    :left left
                                                    :right right)))))
      expression)))

(defun binary-precedence (parser token)
  (with-slots (context) parser
    (let ((precedence))
      (cond
       ((typep token 'punctuator)
        (setf precedence (or (operator-precedence (slot-value token 'value)) 0)))
       ((typep token 'keyword)
        (setf precedence (if (or (equal "instanceof" (slot-value token 'name))
                                 (and (getf context :allow-in)
                                      (equal "in" (slot-value token 'name))))
                             7
                           0)))
       (t (setf precedence 0)))
      precedence)))

(defun parse-binary-expression (parser)
  (with-slots (context lookahead) parser
    (let* ((start-token lookahead)
           (expression (parse-exponentiation-expression parser))
           (token lookahead)
           (precedence (binary-precedence parser token)))
      (when (> precedence 0)
        (next-token parser)
        (setf (getf context :assignment-target-p) nil
              (getf context :binding-element-p) nil)
        (let* ((markers `(,lookahead ,start-token))
               (left expression)
               (right (isolate-cover-grammar parser 'parse-exponentiation-expression))
               (stack `(,right ,(slot-value token 'value) ,left))
               (precedences `(,precedence)))
          (loop
           (let ((precedence (binary-precedence parser lookahead)))
             (when (<= precedence 0)
               (return))
             (loop while (and (> (length stack) 2)
                              (<= precedence (first precedences)))
                   for right = (pop stack)
                   for operator = (pop stack)
                   do (pop precedences)
                   (let ((left (pop stack)))
                     (pop markers)
                     (let ((marker (start-marker (first markers))))
                       (push (finalize parser marker
                                       (make-instance 'binary-expression
                                                      :operator operator
                                                      :left left
                                                      :right right))
                             stack))))
             (push (slot-value (next-token parser) 'value) stack)
             (push precedence precedences)
             (push lookahead markers)
             (push (isolate-cover-grammar parser 'parse-exponentiation-expression) stack)))
          (let ((i 0))
            (setf expression (nth i stack))
            (let ((last-marker (pop markers)))
              (loop while (< i (1- (length stack)))
                    for marker_ = (pop markers)
                    for last-line-start = (and last-marker
                                               (slot-value last-marker 'line-start))
                    for marker = (start-marker  marker_ last-line-start)
                    for operator = (nth (1+ i) stack)
                    do (setf expression (finalize parser marker
                                                  (make-instance 'binary-expression
                                                                 :operator operator
                                                                 :left (nth (+ i 2) stack)
                                                                 :right expression))
                             i (+ i 2)
                             last-marker marker_))))))
      expression)))

(defun parse-conditional-expression (parser)
  (with-slots (lookahead context) parser
    (let ((start-token lookahead)
          (expression (inherit-cover-grammar parser 'parse-binary-expression)))
      (when (match parser "?")
        (next-token parser)
        (let ((previous-allow-in (getf context :allow-in)))
          (setf (getf context :allow-in) t)
          (let ((consequent (isolate-cover-grammar parser 'parse-assignment-expression)))
            (setf (getf context :allow-in) previous-allow-in)
            (expect parser ":")
            (let ((alternate (isolate-cover-grammar parser 'parse-assignment-expression)))
              (setf expression (finalize parser (start-marker start-token)
                                         (make-instance 'conditional-expression
                                                        :test expression
                                                        :consequent consequent
                                                        :alternate alternate))
                    (getf context :assignment-target-p) nil
                    (getf context :binding-element-p) nil)))))
      expression)))

(defun check-pattern-param (parser options param)
  (typecase param
    (identifier
     (validate-param parser options param (slot-value param 'name)))
    (rest-element
     (check-pattern-param parser options (slot-value param 'argument)))
    (assignment-pattern
     (check-pattern-param parser options (slot-value param 'left)))
    (array-pattern
     (let ((elements (slot-value param 'elements)))
       (loop for element in elements
             when element
             do (check-pattern-param parser options element))))
    (object-pattern
     (let ((properties (slot-value param 'properties)))
       (loop for property in properties
             do (check-pattern-param parser options (slot-value property 'value))))))
  (setf (getf options :simple)
        (and (getf options :simple)
             (typep param 'identifier))))

(defun reinterpret-as-cover-formals-list (parser expression)
  (with-slots (lookahead context) parser
    (let ((params `(,expression))
          (options)
          (async-arrow-p))
      (cond 
       ((typep expression 'identifier))
       ((and (listp expression)
             (eq :arrow-parameter-placeholder (getf expression :type)))
        (setf params (getf expression :params)
              async-arrow-p (getf expression :async))))
      (setf (getf options :simple) t
            (getf options :param-set) nil)
      (loop for param in params
            for index from 0
            do (cond
                ((typep param 'assignment-pattern)
                 (when (typep (slot-value param 'right) 'yield-expression)
                   (when (slot-value (slot-value param 'right) 'arguments)
                     (throw-unexpected-token parser lookahead))
                   (change-class (slot-value param 'right) 'identifier)
                   (setf (slot-value (slot-value param 'right) 'name) "yield")))
                ((and async-arrow-p
                      (typep param 'identifier)
                      (equal "await" (slot-value param 'name)))
                 (throw-unexpected-token parser lookahead)))
            (check-pattern-param parser options param)
            (setf (nth index params) param))
      (when (or (getf context :strict)
                (not (getf context :allow-yield)))
        (loop for param in params
              when (typep param 'yield-expression)
              do (throw-unexpected-token parser lookahead)))
      (when (getf options :message)
        (let ((token (if (getf context :strict)
                         (getf options :strict)
                       (getf options :first-restricted))))
          (throw-unexpected-token parser token (getf options :message))))
      `(:simple ,(getf options :simple)
        :params ,params
        :stricted ,(getf options :stricted)
        :first-restricted (getf options :first-restricted)
        :message (getf options :message)))))

(defun parse-assignment-expression (parser)
  (with-slots (lookahead context has-line-terminator-p) parser
    (let ((expression))
      (if (and (not (getf context :allow-yield))
               (match-keyword parser "yield"))
          (setf expression (parse-yield-expression parser))
        (let* ((start-token lookahead)
               (token start-token))
          (setf expression (parse-conditional-expression parser))
          (when (and (typep token 'identifier)
                     (= (slot-value token 'line-number)
                        (slot-value lookahead 'line-number))
                     (equal "async" (slot-value token 'name)))
            (when (or (typep lookahead 'identifier)
                      (match-keyword parser "yield"))
              (let ((argument (parse-primary-expression parser)))
                (reinterpret-expression-as-pattern argument)
                (setf expression `(:type :arrow-parameter-placeholder
                                   :params ,`(,argument)
                                   :async t)))))
          (if (or (and (typep expression 'list)
                       (eq :arrow-parameter-placeholder (getf expression :type)))
                  (match parser "=>"))
              (progn
                (setf (getf context :assignment-target-p) nil
                      (getf context :binding-element-p) nil)
                (let ((async-p (getf expression :async))
                      (list (reinterpret-as-cover-formals-list parser expression)))
                  (when list
                    (when has-line-terminator-p
                      (tolerate-unexpected-token parser lookahead))
                    (setf (getf context :first-cover-initialized-name-error) nil)
                    (let ((previous-strict (getf context :strict))
                          (previous-allow-strict-directive (getf context :allow-strict-directive)))
                      (setf (getf context :allow-strict-directive) (getf list :simple))
                      (let ((previous-allow-yield (getf context :allow-yield))
                            (previous-await (getf context :await)))
                        (setf (getf context :allow-yield) t
                              (getf context :await) async-p)
                        (let ((marker (start-marker start-token)))
                          (expect parser "=>")
                          (let ((body))
                            (if (match parser "{")
                                (let ((previous-allow-in (getf context :allow-in)))
                                  (setf (getf context :allow-in) t
                                        body (parse-function-source-elements parser)
                                        (getf context :allow-in) previous-allow-in))
                              (setf body (isolate-cover-grammar parser 'parse-assignment-expression)))
                            (let ((expression-p (not (typep body 'block-statement))))
                              (when (and (getf context :strict)
                                         (getf list :first-restricted))
                                (throw-unexpected-token parser (getf list :first-restricted) "some message"))
                              (when (and (getf context :strict)
                                         (getf list :stricted))
                                (throw-unexpected-token parser (getf list :stricted) "some message"))
                              (setf expression
                                    (if async-p
                                        (finalize parser marker
                                                  (make-instance 'async-arrow-function-expression
                                                                 :params (getf list :params)
                                                                 :body body
                                                                 :expression expression-p))
                                      (finalize parser marker
                                                (make-instance 'arrow-function-expression
                                                               :params (getf list :params)
                                                               :body body
                                                               :expression expression-p))))
                              (setf (getf context :strict) previous-strict
                                    (getf context :allow-strict-directive) previous-allow-strict-directive
                                    (getf context :allow-yield) previous-allow-yield
                                    (getf context :await) previous-await)))))))))
            (when (match-assign parser)
              (unless (getf context :assignment-target-p)
                (tolerate-error parser "some message"))
              (when (and (getf context :strict)
                         (typep expression 'identifier))
                (let ((id expression))
                  (when (restricted-word-p (slot-value id 'name))
                    (tolerate-unexpected-token parser token "some message"))
                  (when (strict-mode-reserved-word-p (slot-value id 'name))
                    (tolerate-unexpected-token parser token "some message"))))
              (if (not (match parser "="))
                  (setf (getf context :assignment-target-p) nil
                        (getf context :binding-element-p) nil)
                (reinterpret-expression-as-pattern expression))
              (setf token (next-token parser))
              (let ((operator (slot-value token 'value))
                    (right (isolate-cover-grammar parser 'parse-assignment-expression)))
                (setf expression (finalize parser (start-marker start-token)
                                           (make-instance 'assignment-expression
                                                          :operator operator
                                                          :left expression
                                                          :right right))
                      (getf context :first-cover-initialized-name-error) nil))))))
      expression)))

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
          (setf expression (finalize parser (start-marker start-token)
                                     (make-instance 'sequence-expression
                                                    :expressions expressions)))))
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
            ("const" (setf statement (parse-lexical-declaration parser '(:in-for nil))))
            ("function" (setf statement (parse-function-declaration parser)))
            ("class" (setf statement (parse-class-declaration parser)))
            ("let" (setf statement (if (lexical-declaration-p parser)
                                       (parse-lexical-declaration parser '(:in-for nil))
                                     (parse-statement parser))))
            (t (setf statement (parse-statement parser))))
        (setf statement (parse-statement parser)))
      statement)))

(defun parse-block (parser)
  (let ((marker (create-marker parser)))
    (expect parser "{")
    (let ((body))
      (loop
       (when (match parser "}")
         (return))
       (appendf body (list (parse-statement-list-item parser))))
      (expect parser "}")
      (finalize parser marker (make-instance 'block-statement :body body)))))

(defun parse-lexical-binding (parser kind options)
  (with-slots (context) parser
    (let* ((marker (create-marker parser))
           (params)
           (id (parse-pattern parser params kind)))
      (when (and (getf context :strict)
                 (typep id 'identifier))
        (when (restricted-word-p (slot-value id 'name))
          (tolerate-error parser "some message")))
      (let ((init))
        (cond
         ((equal "const" kind)
          (when (and (not (match-keyword parser "in"))
                     (not (match-contextual-keyword parser "of")))
            (if (match parser "=")
                (progn
                  (next-token parser)
                  (setf init (isolate-cover-grammar parser 'parse-assignment-expression)))
              (throw-error parser "some message"))))
         ((or (and (not (getf options :in-for))
                   (not (typep id 'identifier)))
              (match parser "="))
          (expect parser "=")
          (setf init (isolate-cover-grammar parser 'parse-assignment-expression))))
        (finalize parser marker (make-instance 'variable-declarator
                                               :id id
                                               :init init))))))

(defun parse-binding-list (parser kind options)
  (let ((list `(,(parse-lexical-binding parser kind options))))
    (loop while (match parser ",")
          do (next-token parser)
          (appendf list (list (parse-lexical-binding parser kind options))))
    list))

(defun lexical-declaration-p (parser)
  (with-slots (scanner) parser
    (let ((state (save-state scanner)))
      (scan-comments scanner)
      (let ((next-token (lex scanner)))
        (restore-state scanner state)
        (or (typep next-token 'identifier)
            (and (typep next-token 'punctuator)
                 (equal "[" (slot-value next-token 'value)))
            (and (typep next-token 'punctuator)
                 (equal "{" (slot-value next-token 'value)))
            (and (typep next-token 'keyword)
                 (equal "let" (slot-value next-token 'name)))
            (and (typep next-token 'keyword)
                 (equal "yield" (slot-value next-token 'name))))))))

(defun parse-lexical-declaration (parser options)
  (let ((marker (create-marker parser))
        (kind (slot-value (next-token parser) 'value)))
    (let ((declarations (parse-binding-list parser kind options)))
      (consume-semicolon parser)
      (finalize parser marker (make-instance 'variable-declaration
                                             :declarations declarations
                                             :kind kind)))))

(defun parse-binding-rest-element (parser params kind)
  (let ((marker (create-marker parser)))
    (expect parser "...")
    (let ((argument (parse-pattern parser params kind)))
      (finalize parser marker (make-instance 'rest-element :argument argument)))))

(defun parse-array-pattern (parser params kind)
  (let ((marker (create-marker parser)))
    (expect parser "[")
    (let ((elements))
      (loop while (not (match parser "]"))
            do (if (match parser ",")
                   (progn
                     (next-token parser)
                     (appendf elements (list nil)))
                 (if (match parser "...")
                     (progn
                       (appendf elements (list (parse-binding-rest-element parser params kind)))
                       (return))
                   (appendf elements (list (parse-pattern-with-default parser params kind)))))
            (when (not (match parser "]"))
              (expect parser ",")))
      (expect parser "]")
      (finalize parser marker (make-instance 'array-pattern :elements elements)))))

(defun parse-property-pattern (parser params kind)
  (with-slots (lookahead) parser
    (let ((marker (create-marker parser)))
      (let ((computed)
            (shorthand)
            (method)
            (key)
            (value))
        (if (typep lookahead 'identifier)
            (let ((key-token lookahead))
              (setf key (parse-variable-identifier parser))
              (let ((init (finalize parser marker (make-instance 'identifier
                                                                 :name (slot-value key-token 'name)))))
                (cond
                 ((match parser "=")
                  (appendf params (list key-token))
                  (setf shorthand t)
                  (next-token parser)
                  (let ((expression (parse-assignment-expression parser)))
                    (setf value (finalize parser marker (make-instance 'assignment-pattern
                                                                       :left init
                                                                       :right expression)))))
                 ((not (match parser ":"))
                  (appendf params (list key-token))
                  (setf shorthand t
                        value init))
                 (t (expect parser ":")
                    (setf value (parse-pattern-with-default parser params kind))))))
          (progn
            (setf computed (match parser "[")
                  key (parse-object-property-key parser))
            (expect parser ":")
            (setf value (parse-pattern-with-default parser params kind))))
        (finalize parser marker (make-instance 'property
                                               :key key
                                               :computed computed
                                               :value value
                                               :method method
                                               :shorthand shorthand))))))

(defun parse-object-pattern (parser params kind)
  (let ((marker (create-marker parser))
        (properties))
    (expect parser "{")
    (loop while (not (match parser "}"))
          do (appendf properties (parse-property-pattern parser params kind))
          (unless (match parser "}")
            (expect parser ",")))
    (expect parser "}")
    (finalize parser marker (make-instance 'object-pattern :properties properties))))

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
                                                   :right right))))))
      pattern)))

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

(defun parse-variable-declaration (parser &optional options)
  (with-slots (context) parser
    (let ((marker (create-marker parser)))
      (let* ((params)
             (id (parse-pattern parser params "var")))
        (when (and (getf context :strict)
                   (typep id 'identifier))
          (when (restricted-word-p (slot-value id 'name))
            (tolerate-error parser "some message")))
        (let ((init))
          (cond
           ((match parser "=")
            (next-token parser)
            (setf init (isolate-cover-grammar parser 'parse-assignment-expression)))
           ((and (not (typep id 'identifier))
                 (not (getf options :in-for)))
            (expect parser "=")))
          (finalize parser marker (make-instance 'variable-declarator
                                                 :id id
                                                 :init init)))))))

(defun parse-variable-declaration-list (parser &optional options)
  (let ((list))
    (appendf list (list (parse-variable-declaration parser options)))
    (loop while (match parser ",")
          do (next-token parser)
          (appendf list (list (parse-variable-declaration parser options))))
    list))

(defun parse-variable-statement (parser)
  (let ((marker (create-marker parser)))
    (expect-keyword parser "var")
    (let ((declarations (parse-variable-declaration-list parser)))
      (consume-semicolon parser)
      (finalize parser marker
                (make-instance 'variable-declaration
                               :declarations declarations
                               :kind "var")))))

(defun parse-empty-statement (parser)
  (let ((marker (create-marker parser)))
    (expect parser ";")
    (finalize parser marker (make-instance 'empty-statement))))

(defun parse-expression-statement (parser)
  (let ((marker (create-marker parser))
        (expression (parse-expression parser)))
    (consume-semicolon parser)
    (finalize parser marker (make-instance 'expression-statement
                                           :expression expression))))

(defun parse-if-clause (parser)
  (with-slots (context) parser
    (when (and (getf context :strict)
               (match-keyword parser "function"))
      (tolerate-error parser "some message"))
    (parse-statement parser)))

(defun parse-if-statement (parser)
  (with-slots (config) parser
    (let ((marker (create-marker parser))
          (consequent)
          (alternate))
      (expect-keyword parser "if")
      (expect parser "(")
      (let ((test (parse-expression parser)))
        (if (and (not (match parser ")"))
                 (getf config :tolerant))
            (progn
              (tolerate-unexpected-token (next-token parser))
              (setf consequent (finalize parser (create-marker parser)
                                         (make-instance 'empty-statement))))
          (progn
            (expect parser ")")
            (setf consequent (parse-if-clause parser))
            (when (match-keyword parser "else")
              (next-token parser)
              (setf alternate (parse-if-clause parser)))))
        (finalize parser marker (make-instance 'if-statement
                                               :test test
                                               :consequent consequent
                                               :alternate alternate))))))

(defun parse-do-while-statement (parser)
  (with-slots (context config) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "do")
      (let ((previous-in-iteration (getf context :in-iteration)))
        (setf (getf context :in-iteration) t)
        (let ((body (parse-statement parser)))
          (setf (getf context :in-iteration) previous-in-iteration)
          (expect-keyword parser "while")
          (expect parser "(")
          (let ((test (parse-expression parser)))
            (if (and (not (match parser ")"))
                     (getf config :tolerant))
                (tolerate-unexpected-token parser (next-token parser))
              (progn
                (expect parser ")")
                (when (match parser ";")
                  (next-token parser))))
            (finalize parser marker (make-instance 'do-while-statement
                                                   :body body
                                                   :test test))))))))

(defun parse-while-statement (parser)
  (with-slots (context config) parser
    (let ((marker (create-marker parser))
          (body))
      (expect-keyword parser "while")
      (expect parser "(")
      (let ((test (parse-expression parser)))
        (if (and (not (match parser ")"))
                 (getf config :tolerant))
            (progn
              (tolerate-unexpected-token parser (next-token parser))
              (setf body (finalize parser marker (make-instance 'empty-statement))))
          (progn
            (expect parser ")")
            (let ((previous-in-iteration (getf context :in-iteration)))
              (setf (getf context :in-iteration) t
                    body (parse-statement parser)
                    (getf context :in-iteration) previous-in-iteration))))
        (finalize parser marker (make-instance 'while-statement
                                               :test test
                                               :body body))))))

(defun parse-for-statement (parser)
  (with-slots (context lookahead config) parser
    (let ((init)
          (test)
          (update)
          (for-in t)
          (left)
          (right)
          (marker (create-marker parser)))
      (expect-keyword parser "for")
      (expect parser "(")
      (if (match parser ";")
          (next-token parser)  
        (cond
         ((match-keyword parser "var")
          (setf init (create-marker parser))
          (next-token parser)
          (let ((previous-allow-in (getf context :allow-in)))
            (setf (getf context :allow-in) nil)
            (let ((declarations (parse-variable-declaration-list parser '(:in-for t))))
              (setf (getf context :allow-in) previous-allow-in)
              (cond
               ((and (= 1 (length declarations))
                     (match-keyword parser "in"))
                (let ((declaration (first declarations)))
                  (when (and (slot-value declaration 'init)
                             (or (typep (slot-value declaration 'id) 'array-pattern)
                                 (typep (slot-value declaration 'id) 'object-pattern)
                                 (getf context :strict)))
                    (tolerate-error parser "some message"))
                  (setf init (finalize parser init (make-instance 'variable-declaration
                                                                  :declarations declarations
                                                                  :kind "var")))
                  (next-token parser)
                  (setf left init
                        right (parse-expression parser)
                        init nil)))
               ((and (= 1 (length declarations))
                     (null (first declarations))
                     (match-contextual-keyword parser "of"))
                (setf init (finalize parser init (make-instance 'variable-declaration
                                                                :declarations declarations
                                                                :kind "var")))
                (next-token parser)
                (setf left init
                      right (parse-assignment-expression parser)
                      init nil
                      for-in nil))
               (t (setf init (finalize parser init (make-instance 'variable-declarat
                                                                  :declarations declarations
                                                                  :kind "var"))))))))
         ((or (match-keyword parser "const")
              (match-keyword parser "let"))
          (setf init (create-marker parser))
          (let ((kind (slot-value (next-token parser) 'value)))
            (if (and (not (getf context :strict))
                     (equal "in" (slot-value lookahead 'value)))
                (progn
                  (setf init (finalize parser init (make-instance 'identifier :name kind)))
                  (next-token parser)
                  (setf left init
                        right (parse-expression parser)
                        init nil))
              (let ((previous-allow-in (getf context :allow-in)))
                (setf (getf context :allow-in) nil)
                (let ((declarations (parse-binding-list parser kind '(:in-for t))))
                  (setf (getf context :allow-in) previous-allow-in)
                  (cond
                   ((and (= 1 (length declarations))
                         (null (slot-value (first declarations) 'init))
                         (match-keyword parser "in"))
                    (setf init (finalize parser init (make-instance 'variable-declaration
                                                                    :declarations declarations
                                                                    :kind kind)))
                    (next-token parser)
                    (setf left init
                          right (parse-expression parser)
                          init nil))
                   ((and (= 1 (length declarations))
                         (null (slot-value (first declarations) 'init))
                         (match-contextual-keyword parser "of"))
                    (setf init (finalize parser init (make-instance 'variable-declaration
                                                                    :declarations declarations
                                                                    :kind kind)))
                    (next-token parser)
                    (setf left init
                          right (parse-assignment-expression parser)
                          init nil
                          for-in nil))
                   (t (consume-semicolon parser)
                      (setf init (finalize parser init (make-instance 'variable-declaration
                                                                      :declarations declarations
                                                                      :kind kind))))))))))
         (t (let ((init-start-token lookahead)
                  (previous-allow-in (getf context :allow-in)))
              (setf (getf context :allow-in) nil
                    init (inherit-cover-grammar parser 'parse-assignment-expression)
                    (getf context :allow-in) previous-allow-in)
              (cond
               ((match-keyword parser "in")
                (when (or (not (getf context :assignment-target-p))
                          (typep init 'assignment-expression))
                  (tolerate-error parser "some message"))
                (next-token parser)
                (reinterpret-expression-as-pattern init)
                (setf left init
                      right (parse-expression parser)
                      init nil))
               ((match-contextual-keyword parser "of")
                (when (or (not (getf context :assignment-target-p))
                          (typep init 'assignment-expression))
                  (tolerate-error parser "some message"))
                (next-token parser)
                (reinterpret-expression-as-pattern init)
                (setf left init
                      right (parse-assignment-expression parser)
                      init nil
                      for-in nil))
               (t (when (match parser ",")
                    (let ((init-seq `(,init)))
                      (loop while (match parser ",")
                            do (next-token parser)
                            (appendf init-seq (isolate-cover-grammar parser
                                                                     'parse-assignment-expression)))
                      (setf init (finalize parser (start-marker init-start-token)
                                           (make-instance 'sequence-expression
                                                          :expressions init-seq)))))
                  (expect parser ";")))))))
      (when (null left)
        (when (not (match parser ";"))
          (setf test (parse-expression parser)))
        (expect parser ";")
        (when (not (match parser ")"))
          (setf update (parse-expression parser))))
      (let ((body))
        (if (and (not (match parser ")"))
                 (getf config :tolerant))
            (progn
              (tolerate-unexpected-token parser (next-token parser))
              (setf body (finalize parser (create-marker parser)
                                   (make-instance 'empty-statement))))
          (progn
            (expect parser ")")
            (let ((previous-in-iteration (getf context :in-iteration)))
              (setf (getf context :in-iteration) t
                    body (isolate-cover-grammar parser 'parse-statement)
                    (getf context :in-iteration) previous-in-iteration))))
        (if (null left)
            (finalize parser marker (make-instance 'for-statement
                                                   :init init
                                                   :test test
                                                   :update update
                                                   :body body))
          (if for-in
              (finalize parser marker (make-instance 'for-in-statement
                                                     :left left
                                                     :right right
                                                     :body body))
            (finalize parser marker (make-instance 'for-of-statement
                                                   :left left
                                                   :right right
                                                   :body body))))))))

(defun parse-continue-statement (parser)
  (with-slots (lookahead context has-line-terminator-p) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "continue")
      (let ((label))
        (when (and (typep lookahead 'identifier)
                   (not has-line-terminator-p))
          (let ((id (parse-variable-identifier parser)))
            (setf label id)
            (let ((key (format nil "$~A" (slot-value id 'name))))
              (unless (getf (getf context :label-set) key)
                (throw-error parser "some message")))))
        (consume-semicolon parser)
        (when (and (null label)
                   (not (getf context :in-iteration)))
          (throw-error parser "some message"))
        (finalize parser marker (make-instance 'continue-statement
                                               :label label))))))

(defun parse-break-statement (parser)
  (with-slots (lookahead context has-line-terminator-p) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "break")
      (let ((label))
        (when (and (typep lookahead 'identifier)
                   (not has-line-terminator-p))
          (let ((id (parse-variable-identifier parser)))
            (let ((key (format nil "$~A" (slot-value id 'name))))
              (unless (getf (getf context :label-set) key)
                (throw-error parser "some message"))
              (setf label id))))
        (consume-semicolon parser)
        (when (and (null label)
                   (not (getf context :in-iteration))
                   (not (getf context :in-switch)))
          (throw-error parser "some message"))
        (finalize parser marker (make-instance 'break-statement
                                               :label label))))))

(defun parse-return-statement (parser)
  (with-slots (context lookahead has-line-terminator-p) parser
    (unless (getf context :in-function-body)
      (tolerate-error parser "some message"))
    (let ((marker (create-marker parser)))
      (expect-keyword parser "return")
      (let ((has-argument-p (or (and (not (match parser ";"))
                                     (not (match parser "}"))
                                     (not has-line-terminator-p)
                                     (not (typep lookahead 'eof)))
                                (typep lookahead 'string-literal)
                                (typep lookahead 'template))))
        (let ((argument (when has-argument-p
                          (parse-expression parser))))
          (consume-semicolon parser)
          (finalize parser marker (make-instance 'return-statement
                                                 :argument argument)))))))

(defun parse-with-statement (parser)
  (with-slots (context config) parser
    (unless (getf context :strict)
      (tolerate-error parser "some message"))
    (let ((marker (create-marker parser))
          (body))
      (expect-keyword parser "with")
      (expect parser "(")
      (let ((object (parse-expression parser)))
        (if (and (not (match parser ")"))
                 (getf config :tolerant))
            (progn
              (tolerate-unexpected-token parser (next-token parser))
              (setf body (finalize parser (create-marker parser)
                                   (make-instance 'empty-statement))))
          (progn
            (expect parser ")")
            (setf body (parse-statement parser))))
        (finalize parser marker (make-instance 'with-statement
                                               :object object
                                               :body body))))))

(defun parse-switch-case (parser)
  (let ((marker (create-marker parser))
        (test))
    (if (match-keyword parser "default")
        (progn
          (next-token parser)
          (setf test nil))
      (progn
        (expect-keyword parser "case")
        (setf test (parse-expression parser))))
    (expect parser ":")
    (let ((consequent))
      (loop
       (when (or (match parser "}")
                 (match-keyword parser "default")
                 (match-keyword parser "case"))
         (return))
       (appendf consequent (list (parse-statement-list-item parser))))
      (finalize parser marker (make-instance 'switch-case
                                             :test test
                                             :consequent consequent)))))

(defun parse-switch-statement (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "switch")
      (expect parser "(")
      (let ((discriminant (parse-expression parser)))
        (expect parser ")")
        (let ((previous-in-switch (getf context :in-switch)))
          (setf (getf context :in-switch) t)
          (let ((cases)
                (default-found-p))
            (expect parser "{")
            (loop
             (when (match parser "}")
               (return))
             (let ((clause (parse-switch-case parser)))
               (when (null (slot-value clause 'test))
                 (when default-found-p
                   (throw-error parser "some message"))
                 (setf default-found-p t))
               (appendf cases (list clause))))
            (expect parser "}")
            (setf (getf context :in-switch) previous-in-switch)
            (finalize parser marker (make-instance 'switch-statement
                                                   :discriminant discriminant
                                                   :cases cases))))))))

(defun parse-labelled-statement (parser)
  (with-slots (context lookahead) parser
    (let ((marker (create-marker parser))
          (expression (parse-expression parser))
          (statement))
      (if (and (typep expression 'identifier)
               (match parser ":"))
          (progn
            (next-token parser)
            (let* ((id expression)
                   (key (format nil "$~A" (slot-value id 'name))))
              (when (getf (getf context :label-set) key)
                (throw-error parser "some message"))
              (setf (getf (getf context :label-set) key) t)
              (let ((body))
                (cond
                 ((match-keyword parser "class")
                  (tolerate-unexpected-token parser lookahead)
                  (setf body (parse-class-declaration parser)))
                 ((match-keyword parser "function")
                  (let ((token lookahead)
                        (declaration (parse-function-declaration parser)))
                    (if (getf context :strict)
                        (tolerate-unexpected-token parser token "some message")
                      (if (slot-value declaration 'generator)
                          (tolerate-unexpected-token parser token "some message")))))
                 (t (setf body (parse-statement parser))))
                (setf (getf (getf context :label-set) key) nil)
                (setf statement (make-instance 'labeled-statement
                                               :label id
                                               :body body)))))
        (progn
          (consume-semicolon parser)
          (setf statement (make-instance 'expression-statement
                                         :expression expression))))
      (finalize parser marker statement))))

(defun parse-throw-statement (parser)
  (with-slots (has-line-terminator-p) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "throw")
      (when has-line-terminator-p
        (throw-error parser "some message"))
      (let ((argument (parse-expression parser)))
        (consume-semicolon parser)
        (finalize parser marker (make-instance 'throw-statement :argument argument))))))

(defun parse-catch-clause (parser)
  (with-slots (lookahead context) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "catch")
      (expect parser "(")
      (when (match parser ")")
        (throw-unexpected-token parser lookahead))
      ;; TODO: check this
      (let* ((params)
             (param (parse-pattern parser params))
             (param-map))
        (loop for param in params
              for key = (format nil "$~A" param)
              do (when (getf param-map key) ;
                   (tolerate-error parser "some message")
                   (setf (getf param-map key) t)))
        (when (and (getf context :strict)
                   (typep param 'identifier))
          (when (restricted-word-p (slot-value param 'name))
            (tolerate-error parser "some message")))
        (expect parser ")")
        (let ((body (parse-block parser)))
          (finalize parser marker (make-instance 'catch-clause
                                                 :param param
                                                 :body body)))))))

(defun parse-finally-clause (parser)
  (expect-keyword parser "finally")
  (parse-block parser))

(defun parse-try-statement (parser)
  (let ((marker (create-marker parser)))
    (expect-keyword parser "try")
    (let ((block (parse-block parser))
          (handler (when (match-keyword parser "catch")
                     (parse-catch-clause parser)))
          (finalizer (when (match-keyword parser "finally")
                       (parse-finally-clause parser))))
      (when (and (not handler) (not finalizer))
        (throw-error parser "some message"))
      (finalize parser marker (make-instance 'try-statement
                                             :block block
                                             :handler handler
                                             :finalizer finalizer)))))

(defun parse-debugger-statement (parser)
  (let ((marker (create-marker parser)))
    (expect-keyword parser "debugger")
    (consume-semicolon parser)
    (finalize parser marker (make-instance 'debugger-statement))))

(defun parse-statement (parser)
  (with-slots (lookahead) parser
    (let ((statement))
      (typecase lookahead
        ((or boolean-literal null-literal numeric-literal
             string-literal template-literal reg-exp-literal)
         (setf statement (parse-expression-statement parser)))
        (punctuator
         (switch ((slot-value lookahead 'value) :test 'equal)
           ("{" (setf statement (parse-block parser)))
           ("(" (setf statement (parse-expression-statement parser)))
           (";" (setf statement (parse-empty-statement parser)))
           (t (setf statement (parse-expression-statement parser)))))
        (identifier
         (setf statement (if (match-async-function parser)
                             (parse-function-declaration parser)
                           (parse-labelled-statement parser))))
        (keyword
         (switch ((slot-value lookahead 'name) :test 'equal)
           ("break" (setf statement (parse-break-statement parser)))
           ("continue" (setf statement (parse-continue-statement parser)))
           ("debugger" (setf statement (parse-debugger-statement parser)))
           ("do" (setf statement (parse-do-while-statement parser)))
           ("for" (setf statement (parse-for-statement parser)))
           ("function" (setf statement (parse-function-declaration parser)))
           ("if" (setf statement (parse-if-statement parser)))
           ("return" (setf statement (parse-return-statement parser)))
           ("switch" (setf statement (parse-switch-statement parser)))
           ("throw" (setf statement (parse-throw-statement parser)))
           ("try" (setf statement (parse-try-statement parser)))
           ("var" (setf statement (parse-variable-statement parser)))
           ("while" (setf statement (parse-while-statement parser)))
           ("with" (setf statement (parse-with-statement parser)))
           (t (setf statement (parse-expression-statement parser)))))
        (t (setf statement (throw-unexpected-token parser lookahead))))
      statement)))

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
              do (if (match parser "}")
                     (return)
                   (if-let ((statement (parse-statement-list-item parser)))
                       (appendf body (list statement))
                     (return))))
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

(defun match-async-function (parser)
  (with-slots (scanner) parser
    (let ((match-p (match-contextual-keyword parser "async")))
      (when match-p
        (let ((state (save-state scanner)))
          (scan-comments scanner)
          (let ((next-token (lex scanner)))
            (restore-state scanner state)
            (setf match-p (and (eq (getf state :line-number)
                                   (slot-value next-token 'line-number))
                               (typep next-token 'keyword)
                               (equal "function" (slot-value next-token 'name)))))))
      match-p)))

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

(defun parse-function-expression (parser)
  (with-slots (context lookahead) parser
    (let ((marker (create-marker parser))
          (async-p (match-contextual-keyword parser "async")))
      (when async-p
        (next-token parser))
      (expect-keyword parser "function")
      (let ((generator-p (if async-p nil (match parser "*"))))
        (when generator-p
          (next-token parser))
        (let ((message)
              (id)
              (first-restricted)
              (previous-allow-await (getf context :await))
              (previous-allow-yield (getf context :allow-yield)))
          (setf (getf context :await) async-p
                (getf context :allow-yield) (not generator-p))
          (unless (match parser "(")
            (let ((token lookahead))
              (setf id (if (and (not (getf context :strict))
                                (not generator-p)
                                (match-keyword parser "yield"))
                           (parse-identifier-name parser)
                         (parse-variable-identifier parser)))
              (if (getf context :strict)
                  (when (restricted-word-p (slot-value token 'value))
                    (tolerate-unexpected-token parser token "some message"))
                (cond
                 ((restricted-word-p (slot-value token 'value))
                  (setf first-restricted token
                        message "some message"))
                 ((strict-mode-reserved-word-p (slot-value token 'value))
                  (setf first-restricted token
                        message "some message"))))))
          (let* ((formal-parameters (parse-formal-parameters parser first-restricted))
                 (params (slot-value formal-parameters 'params))
                 (stricted (slot-value formal-parameters 'stricted)))
            (setf first-restricted (slot-value formal-parameters 'first-restricted))
            (when (slot-value formal-parameters 'message)
              (setf message (slot-value formal-parameters 'message)))
            (let ((previous-strict (getf context :strict))
                  (previous-allow-strict-directive (slot-value 'formal-parameters 'simple))
                  (body (parse-function-source-elements parser)))
              (when (and (getf context :strict)
                         first-restricted)
                (throw-unexpected-token parser first-restricted message))
              (when (and (getf context :strict)
                         stricted)
                (throw-unexpected-token parser stricted message))
              (setf (getf context :strict) previous-strict
                    (getf context :allow-strict-directive) previous-allow-strict-directive
                    (getf context :await) previous-allow-await
                    (getf context :allow-yield) previous-allow-yield)
              (if async-p
                  (finalize parser marker (make-instance 'async-function-expression
                                                         :id id
                                                         :params params
                                                         :body body))
                (finalize parser marker (make-instance 'function-expression
                                                       :id id
                                                       :params params
                                                       :body body
                                                       :generator generator-p))))))))))

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

(defun qualified-property-name-p (token)
  (typecase token
    ((or identifier string-literal boolean-literal
         null-literal numeric-literal keyword) t)
    (punctuator
     (equal "[" (slot-value token 'value)))))

(defun parse-getter-method (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (generator-p)
          (previous-allow-yield (getf context :allow-yield)))
      (setf (getf context :allow-yield) (not generator-p))
      (let ((formal-parameters (parse-formal-parameters parser)))
        (when (> (length (slot-value formal-parameters 'params)) 0)
          (tolerate-error parser "some message"))
        (let ((method (parse-property-method parser formal-parameters)))
          (setf (getf context :allow-yield) previous-allow-yield)
          (finalize parser marker (make-instance 'function-expression
                                                 :id nil
                                                 :params (slot-value formal-parameters 'params)
                                                 :body method
                                                 :generator generator-p)))))))

(defun parse-setter-method (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (generator-p)
          (previous-allow-yield (getf context :allow-yield)))
      (setf (getf context :allow-yield) (not generator-p))
      (let ((formal-parameters (parse-formal-parameters parser)))
        (cond
         ((not (= 1 (length (slot-value formal-parameters 'params))))
          (tolerate-error parser "some message"))
         ((typep (first (slot-value formal-parameters 'params)) 'rest-element)
          (tolerate-error parser "some message")))
        (let ((method (parse-property-method parser formal-parameters)))
          (setf (getf context :allow-yield) previous-allow-yield)
          (finalize parser marker (make-instance 'function-expression
                                                 :id nil
                                                 :params (slot-value formal-parameters 'params)
                                                 :body method
                                                 :generator generator-p)))))))

(defun parse-generator-method (parser)
  (with-slots (context) parser
    (let ((marker (create-marker parser))
          (generator-p t)
          (previous-allow-yield (getf context :allow-yield)))
      (setf (getf context :allow-yield) t)
      (let ((formal-parameters (parse-formal-parameters parser)))
        (setf (getf context :allow-yield) nil)
        (let ((method (parse-property-method parser formal-parameters)))
          (setf (getf context :allow-yield) previous-allow-yield)
          (finalize parser marker (make-instance 'function-expression
                                                 :id nil
                                                 :params (slot-value formal-parameters 'params)
                                                 :body method
                                                 :generator generator-p)))))))

(defun start-of-expression-p (parser)
  (with-slots (lookahead) parser
    (let ((start-p t))
      (typecase lookahead
        (punctuator
         (let ((value (slot-value lookahead 'value)))
           (setf start-p (member value '("[" "(" "{" "+" "-" "!" "~"
                                         "++" "--" "/" "/=")))))
        (keyword
         (let ((name (slot-value lookahead 'name)))
           (setf start-p (member name '("class" "delete" "function"
                                        "let" "new" "super" "this" "typeof"
                                        "void" "yield"))))))
      start-p)))

(defun parse-yield-expression (parser)
  (with-slots (context has-line-terminator-p) parser
    (let ((marker (create-marker parser)))
      (expect-keyword parser "yield")
      (let ((argument)
            (delegate))
        (unless has-line-terminator-p
          (let ((previous-allow-yield (getf context :allow-yield)))
            (setf (getf context :allow-yield) nil
                  delegate (match parser "*"))
            (cond
             (delegate
              (next-token parser)
              (setf argument (parse-assignment-expression parser)))
             ((start-of-expression-p parser)
              (setf argument (parse-assignment-expression parser))))
            (setf (getf context :allow-yield) previous-allow-yield)))
        (finalize parser marker (make-instance 'yield-expression
                                               :argument argument
                                               :delegate delegate))))))

(defun parse-class-element (parser has-constructor)
  (with-slots (context lookahead has-line-terminator-p) parser
    (let ((token lookahead)
          (marker (create-marker parser))
          (kind)
          (key)
          (value)
          (computed)
          (method)
          (static-p)
          (async-p))
      (if (match parser "*")
          (next-token parser)
        (progn
          (setf computed (match parser "[")
                key (parse-object-property-key parser))
          (let ((id key))
            (when (and (equal "static" (slot-value id 'name))
                       (or (qualified-property-name-p lookahead)
                           (match parser "*")))
              (setf token lookahead
                    static-p t
                    computed (match parser "["))
              (if (match parser "*")
                  (next-token parser)
                (setf key (parse-object-property-key parser))))
            (when (and (typep token 'identifier)
                       (not has-line-terminator-p)
                       (equal "async" (slot-value token 'value)))
              (let ((punctuator (slot-value lookahead 'value)))
                (when (and (not (equal ":" punctuator))
                           (not (equal "(" punctuator))
                           (not (equal "*" punctuator)))
                  (setf async-p t
                        token lookahead
                        key (parse-object-property-key parser))
                  (when (and (typep token 'identifier)
                             (equal "constructor" (slot-value token 'name)))
                    (tolerate-unexpected-token parser token "some message"))))))))
      (let ((lookahead-property-key (qualified-property-name-p lookahead)))
        (cond
         ((typep token 'identifier)
          (cond
           ((and (equal "get" (slot-value token 'name))
                 lookahead-property-key)
            (setf kind "get"
                  computed (match parser "[")
                  key (parse-object-property-key parser)
                  (getf context :allow-yield) nil
                  value (parse-getter-method parser)))
           ((and (equal "set" (slot-value token 'name))
                 lookahead-property-key)
            (setf kind "set"
                  computed (match parser "[")
                  key (parse-object-property-key parser)
                  (getf context :allow-yield) nil
                  value (parse-setter-method parser)))))
         ((and (typep token 'punctuator)
               (equal "*" value)
               lookahead-property-key)
          (setf kind "init"
                computed (match parser "[")
                key (parse-object-property-key parser)
                value (parse-generator-method parser)
                method t)))
        (when (and (not kind)
                   (match parser "("))
          (setf kind "init"
                value (if async-p
                          (parse-property-method-async-function parser)
                        (parse-property-method-function parser))
                method t))
        (unless kind
          (throw-unexpected-token parser lookahead))
        (when (equal "init" kind)
          (setf kind "method"))
        (unless computed
          (when (and static-p (property-key-p key "prototype"))
            (throw-unexpected-token parser token "some message"))
          (when (and (not static-p)
                     (property-key-p key "constructor"))
            (when (or (not (equal "method" kind))
                      (not method)
                      (and value (getf value 'generator)))
              (throw-unexpected-token parser token "some message"))
            (when (slot-value has-constructor 'value)
              (throw-unexpected-token parser token "some message"))
            (setf kind "constructor"))))
      (finalize parser marker (make-instance 'method-definition
                                             :key key
                                             :value value
                                             :kind kind
                                             :computed computed
                                             :static static-p)))))

(defun parse-class-element-list (parser)
  (let ((body)
        (has-constructor '(:value nil)))
    (expect parser "{")
    (loop while (not (match parser "}"))
          do (if (match parser ";")
                 (next-token parser)
               (appendf body (list (parse-class-element parser has-constructor)))))
    (expect parser "}")
    body))

(defun parse-class-body (parser)
  (let ((marker (create-marker parser)))
    (let ((element-list (parse-class-element-list parser)))
      (finalize parser marker (make-instance 'class-body
                                             :body element-list)))))

(defun parse-class-declaration (parser &optional identifier-optional-p)
  (with-slots (context lookahead) parser
    (let ((marker (create-marker parser))
          (previous-strict (getf context :strict)))
      (setf (getf context :strict) t)
      (expect-keyword parser "class")
      (let ((id (and identifier-optional-p
                     (if (typep lookahead 'identifier)
                         nil
                       (parse-variable-identifier parser))))
            (super-class))
        (when (match-keyword parser "extends")
          (next-token parser)
          (setf super-class (isolate-cover-grammar parser 'parse-left-hand-side-expression-allow-call)))
        (let ((class-body (parse-class-body parser)))
          (setf (getf context :strict) previous-strict)
          (finalize parser marker (make-instance 'class-declaration
                                                 :id id
                                                 :super-class super-class
                                                 :body class-body)))))))

(defun parse-class-expression (parser)
  (with-slots (context lookahead) parser
    (let ((marker (create-marker parser))
          (previous-strict (getf context :strict)))
      (setf (getf context :strict) t)
      (expect-keyword parser "class")
      (let ((id (when (typep lookahead 'identifier)
                  (parse-variable-identifier parser)))
            (super-class))
        (when (match-keyword parser "extends")
          (next-token parser)
          (setf super-class (isolate-cover-grammar parser
                                                   'parse-left-hand-side-expression-allow-call)))
        (let ((class-body (parse-class-body parser)))
          (setf (getf context :strict) previous-strict)
          (finalize parser marker (make-instance 'class-expression
                                                 :id id
                                                 :super-class super-class
                                                 :body class-body)))))))

(defun parse-module (parser)
  (with-slots (context lookahead scanner) parser
    (setf (getf context :strict) t
          (getf context :module-p) t
          (slot-value scanner 'module-p) t)
    (let ((marker (create-marker parser))
          (body (parse-directive-prologues parser)))
      (loop while (and lookahead (not (typep lookahead 'eof)))
            do (appendf body (list (parse-statement-list-item parser))))
      (finalize parser marker (make-instance 'module :body body)))))

(defun parse-script (parser)
  (with-slots (lookahead) parser
    (let ((marker (create-marker parser))
          (body (parse-directive-prologues parser)))
      (loop while (and lookahead (not (typep lookahead 'eof)))
            for statement = (parse-statement-list-item parser)
            while statement
            do (appendf body (list statement)))
      (finalize parser marker (make-instance 'script :body body)))))

(defun parse-module-specifier (parser)
  (with-slots (lookahead) parser
    (let ((marker (create-marker parser)))
      (when (typep lookahead 'string-literal)
        (throw-error parser "some message"))
      (let* ((token (next-token parser))
             (raw (get-token-raw parser token)))
        (finalize parser marker (make-instance 'literal
                                               :value (slot-value token 'value)))))))

;; import {<foo as bar>} ...;
(defun parse-import-specifier (parser)
  (with-slots (lookahead) parser
    (let ((marker (create-marker parser))
          (imported)
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
            (throw-unexpected-token parser (next-token parser)))))
      (finalize parser marker (make-instance 'import-specifier
                                             :local local
                                             :imported imported)))))

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
  (let ((marker (create-marker parser))
        (local (parse-identifier-name parser)))
    (finalize parser marker (make-instance 'import-default-specifier
                                           :local local))))

;; import <* as foo> ...;
(defun parse-import-namespace-specifier (parser)
  (let ((marker (create-marker parser)))
    (expect parser "*")
    (unless (match-contextual-keyword parser "as")
      (throw-error parser "some message"))
    (next-token parser)
    (let ((local (parse-identifier-name parser)))
      (finalize parser marker (make-instance 'import-namespace-specifier
                                             :local local)))))

(defun parse-import-declaration (parser)
  (with-slots (lookahead context) parser
    (when (getf context :in-function-body)
      (throw-error parser "some message"))
    (expect-keyword parser "import")
    (let ((marker (create-marker parser))
          (source)
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
               (t (throw-unexpected-token parser lookahead)))))
           (t (throw-unexpected-token parser (next-token parser))))
          (unless (match-contextual-keyword parser "from")
            (throw-error parser "some message"))
          (next-token parser)
          (setf source (parse-module-specifier parser))))
      (consume-semicolon parser)
      (finalize parser marker (make-instance 'import-declaration
                                             :specifiers specifiers
                                             :source source)))))

(defun parse-export-specifier (parser)
  (let ((marker (create-marker parser))
        (local (parse-identifier-name parser)))
    (let ((exported local))
      (when (match-contextual-keyword parser "as")
        (next-token parser)
        (setf exported (parse-identifier-name parser)))
      (finalize parser marker (make-instance 'export-specifier
                                             :local local
                                             :exported exported)))))

(defun parse-export-declaration (parser)
  (with-slots (lookahead context) parser
    (when (getf context :in-function-body)
      (throw-error parser "some message"))
    (expect-keyword parser "export")
    (let ((marker (create-marker parser))
          (export-declaration))
      (cond
       ((match-keyword parser "default")
        ;; export default ...
        (next-token parser)
        (cond
         ((match-keyword parser "function")
          ;; export default function foo () {}
          ;; export default function () {}
          (let ((declaration (parse-function-declaration parser t)))
            (setf export-declaration (finalize parser marker
                                               (make-instance 'export-default-declaration
                                                              :declaration declaration)))))
         ((match-keyword parser "class")
          ;; export default class foo {}
          (let ((declaration (parse-class-declaration parser t)))
            (setf export-declaration (finalize parser marker
                                               (make-instance 'export-default-declaration
                                                              :declaration declaration)))))
         ((match-contextual-keyword parser "async")
          ;; export default async function foo () {}
          ;; export default async function () {}
          ;; export default async x => x
          (let ((declaration (if (match-async-function parser)
                                 (parse-function-declaration parser t)
                               (parse-assignment-expression parser))))
            (setf export-declaration (finalize parser marker
                                               (make-instance 'export-default-declaration
                                                              :declaration declaration)))))
         (t (when (match-contextual-keyword parser "from")
              (throw-error parser "some message"))
            ;; export default {};
            ;; export default [];
            ;; export default (1 + 2);
            (let ((declaration (if (match parser "{")
                                   (parse-object-initializer parser)
                                 (if (match parser "[")
                                     (parse-array-initializer parser)
                                   (parse-assignment-expression parser)))))
              (consume-semicolon parser)
              (setf export-declaration (finalize parser marker
                                                 (make-instance 'export-default-declaration
                                                                :declaration declaration)))))))
       ((match parser "*")
        ;; export * from 'foo';
        (next-token parser)
        (unless (match-contextual-keyword parser "from")
          (error "some error"))
        (next-token parser)
        (let ((source (parse-module-specifier parser)))
          (consume-semicolon parser)
          (setf export-declaration (finalize parser marker
                                             (make-instance 'export-all-declaration
                                                            :source source)))))
       ((typep lookahead 'keyword)
        ;; export var foo = 1;
        (let ((declaration))
          (switch ((slot-value lookahead 'value) :test 'equal)
            ((or "let" "const")
             (setf declaration (parse-lexical-declaration parser '(:in-for nil))))
            ((or "var" "class" "function")
             (setf declaration (parse-statement-list-item parser)))
            (t (throw-unexpected-token parser lookahead)))
          (setf export-declaration (finalize parser marker
                                             (make-instance 'export-named-declaration
                                                            :declaration declaration)))))
       ((match-async-function parser)
        (let ((declaration (parse-function-declaration parser)))
          (setf export-declaration (finalize parser marker
                                             (make-instance 'export-named-declaration
                                                            :declaration declaration)))))
       (t (let ((specifiers)
                (source)
                (export-from-identifier-p))
            (expect parser "{")
            (loop while (not (match parser "}"))
                  do (setf export-from-identifier-p
                           (or export-from-identifier-p
                               (match-keyword parser "default")))
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
            (setf export-declaration (finalize parser marker
                                               (make-instance 'export-named-declaration
                                                              :specifiers specifiers
                                                              :source source))))))
      export-declaration)))

(defun trace-parser ()
  (trace parse-primary-expression
         parse-spread-element
         parse-array-initializer
         parse-property-method
         parse-property-method-function
         parse-property-method-async-function
         parse-object-property-key
         parse-object-property
         parse-object-initializer
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
         parse-catch-clause
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
