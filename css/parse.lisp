(in-package :css)

(define-condition syntax-error (error) ())

(defclass parser ()
  ((stream
    :initarg :stream
    :initform nil)
   (tokenizer
    :initarg :tokenizer
    :initform nil)
   (current-input-token
    :initform nil
    :reader current-input-token)
   (next-input-token
    :initform nil)))

(defgeneric next-input-token (parser)
  (:method ((parser parser))
   (with-slots (tokenizer next-input-token) parser
     (or next-input-token
         (setf next-input-token (consume-token tokenizer))))))

(defgeneric consume-next-input-token (parser)
  (:method ((parser parser))
   (with-slots (tokenizer current-input-token next-input-token) parser
     (let ((token (if next-input-token
                      next-input-token
                    (consume-token tokenizer))))
       (setf next-input-token nil
             current-input-token token)))))

(defgeneric reconsume-current-input-token (parser)
  (:method ((parser parser))
   (with-slots (current-input-token next-input-token) parser
     (setf next-input-token current-input-token))))

(defclass token-parser ()
  ((tokens
    :initarg :tokens
    :initform nil)
   (current-input-token
    :initform nil
    :reader current-input-token)))

(defmethod next-input-token ((parser token-parser))
  (with-slots (tokens) parser
    (first tokens)))

(defmethod consume-next-input-token ((parser token-parser))
  (with-slots (tokens current-input-token) parser
    (setf current-input-token (pop tokens))))

(defmethod reconsume-current-input-token ((parser token-parser))
  (with-slots (tokens current-input-token) parser
    (setf tokens (cons current-input-token tokens))))

(defstruct function (name) (value))

(defstruct simple-block (associated-token) (value))

(defun parse-stylesheet (parser)
  (consume-list-of-rules parser))

(defun parser-list-of-rules (parser)
  (consume-list-of-rules parser))

(defun parse-rule (parser)
  (loop while (whitespace-token-p (next-input-token parser))
        do (consume-next-input-token parser))
  (when (null (next-input-token parser))
    (error 'syntax-error))
  (prog1
      (if (at-keyword-token-p (next-input-token parser))
          (consume-next-input-token parser)
        (or (consume-qualified-rule parser) (error 'syntax-error)))
    (loop while (whitespace-token-p (next-input-token parser))
          do (consume-next-input-token parser))
    (unless (null (next-input-token parser)) (error 'syntax-error))))

(defun parse-declaration (parser)
  (loop for token = (next-input-token parser)
        do (cond
            ((whitespace-token-p token) (consume-next-input-token parser))
            ((not (ident-token-p token)) (error 'syntax-error))
            (t (if-let ((declaration (consume-declaration parser)))
                 (return declaration)
                 (error 'syntax-error))))))

(defun parse-list-of-declarations (parser)
  (consume-list-of-declarations parser))

(defun parse-component-value (parser)
  (loop while (whitespace-token-p (next-input-token parser))
        do (consume-next-input-token parser))
  (when (null (next-input-token parser))
    (error 'syntax-error))
  (prog1
      (consume-component-value parser)
    (loop while (whitespace-token-p (next-input-token parser))
        do (consume-next-input-token parser))
    (unless (null (next-input-token parser))
      (error 'syntax-error))))

(defun parse-list-of-component-values (parser)
  (loop for component-value = (consume-component-value parser)
        until (null component-value)
        collect component-value))

(defun parse-comma-separated-list-of-component-values (parser)
  (loop for component-value = (consume-component-value parser)
        until (or (null component-value)
                  (comma-token-p parser))
        collect component-value into list-of-cvls
        do (when (comma-token-p parser) (continue))
        finally (when (comma-token-p (car (last list-of-cvls)))
                  (setf list-of-cvls (butlast list-of-cvls)))
        (return list-of-cvls)))

(defun consume-list-of-rules (parser)
  (let ((rules))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((whitespace-token-p token))
              ((null token) (return rules))
              ((at-keyword-token-p token)
               (reconsume-current-input-token parser)
               (appendf rules (list (consume-at-rule parser))))
              (t (reconsume-current-input-token parser)
                 (appendf rules (list (consume-qualified-rule parser))))))))

(defun consume-at-rule (parser)
  (let ((name (consume-next-input-token parser))
        (prelude)
        (block))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((semicolon-token-p token) (return `(,name ,prelude)))
              ((null token) (return `(,name ,prelude)))
              ((left-curly-bracket-token-p token)
               (setf block (consume-simple-block parser))
               (return `(,name ,prelude ,block)))
              ((and (simple-block-p token)
                    (left-curly-bracket-token-p
                     (simple-block-associated-token token)))
               (setf block token)
               (return `(,name ,prelude ,block)))
              (t (reconsume-current-input-token parser)
                 (appendf prelude
                          (list (consume-component-value parser))))))))

(defun consume-qualified-rule (parser)
  (let ((prelude)
        (block))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((null token) (return))
              ((left-curly-bracket-token-p token)
               (setf block (consume-simple-block parser))
               (return `(,prelude ,block)))
              ((and (simple-block-p token)
                    (left-curly-bracket-token-p
                     (simple-block-associated-token token)))
               (setf block token)
               (return `(,prelude ,block)))
              (t (reconsume-current-input-token parser)
                 (appendf prelude (list (consume-component-value parser))))))))

(defun consume-list-of-declarations (parser)
  (let ((declarations '()))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((or (whitespace-token-p token)
                   (semicolon-token-p token)))
              ((null token) (return-from consume-list-of-declarations declarations))
              ((at-keyword-token-p token)
               (reconsume-current-input-token parser)
               (appendf declarations (list (consume-at-rule parser))))
              ((ident-token-p token)
               (let ((temporary-list `(,token)))
                 (loop while (and (not (semicolon-token-p (next-input-token parser)))
                                  (not (null (next-input-token parser))))
                       do (appendf temporary-list
                                   (list (consume-component-value parser))))
                 (appendf declarations
                          (list
                           (consume-declaration (make-instance 'token-parser
                                                               :tokens temporary-list))))))
              (t (reconsume-current-input-token parser)
                 (loop while (and (not (semicolon-token-p (next-input-token parser)))
                                  (not (null (next-input-token parser))))
                       do (consume-component-value parser)))))))

(defun consume-declaration (parser)
  (let ((name (consume-next-input-token parser))
        (value '()))
    (loop while (whitespace-token-p (next-input-token parser))
          do (consume-next-input-token parser))
    (if (not (colon-token-p (next-input-token parser)))
        (return-from consume-declaration)
      (consume-next-input-token parser))
    (loop while (whitespace-token-p (next-input-token parser))
          do (consume-next-input-token parser))
    (loop while (next-input-token parser)
          do (appendf value (list (consume-component-value parser))))
    (let ((last-two-tokens (take 2 (reverse (remove 'whitespace-token value
                                                    :key 'type-of)))))
      (when (and (delim-token-p (first last-two-tokens))
                 (eq "!" (delim-token-value (first last-two-tokens)))
                 (ident-token-p (second last-two-tokens))
                 (eq "important" (ident-token-value (second last-two-tokens))))
        (progn
          (setf value (remove (first last-two-tokens) value))
          (setf value (remove (second last-two-tokens) value))))
      (loop for token in (reverse value)
            while (whitespace-token-p token)
            do (setf value (remove token value))))
    `(,name ,value)))

(defun consume-component-value (parser)
  (let ((token (consume-next-input-token parser)))
    (cond
     ((or (left-parenthesis-token-p token)
          (left-square-bracket-token-p token)
          (left-curly-bracket-token-p token))
      (consume-simple-block parser))
     ((function-token-p token)
      (consume-function parser))
     (t token))))

(defun consume-simple-block (parser)
  (let ((ending-token-type
         (typecase (current-input-token parser)
           (left-parenthesis-token 'right-parenthesis-token)
           (left-square-bracket-token 'right-square-bracket-token)
           (left-curly-bracket-token 'right-curly-bracket-token)))
        (simple-block (make-simple-block
                       :associated-token (current-input-token parser))))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((typep token ending-token-type) (return simple-block))
              ((null token) (return simple-block))
              (t (reconsume-current-input-token parser)
                 (appendf (simple-block-value simple-block)
                          (list (consume-component-value parser))))))))

(defun consume-function (parser)
  (let* ((name (current-input-token parser))
         (function (make-function :name name)))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((right-parenthesis-token-p token) (return function))
              ((null token) (return function))
              (t (reconsume-current-input-token parser)
                 (appendf (function-value function)
                          (list (consume-component-value parser))))))))



(defun test-parser-1 ()
  (with-input-from-string (stream "background: red")
    (let* ((tokenizer (make-instance 'tokenizer :stream stream))
           (parser (make-instance 'parser :tokenizer tokenizer :stream stream)))
      (parse-declaration parser))))

(defun test-parser-2 ()
  (with-input-from-string (stream "background: red; margin: 10px 20px;")
    (let* ((tokenizer (make-instance 'tokenizer :stream stream))
           (parser (make-instance 'parser :tokenizer tokenizer :stream stream)))
      (parse-list-of-declarations parser))))

(defun test-parser-3 ()
  (with-input-from-string (stream ".foo, .bar { background: red; margin: 10px 20px; }")
    (let* ((tokenizer (make-instance 'tokenizer :stream stream))
           (parser (make-instance 'parser :tokenizer tokenizer :stream stream)))
      (parse-rule parser))))

