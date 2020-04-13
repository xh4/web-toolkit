(in-package :css)

;; https://www.w3.org/TR/css-syntax-3/#tokenizing-and-parsing

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

(defmethod initialize-instance :after ((parser parser) &key)
  (with-slots (tokenizer stream) parser
    (unless tokenizer
      (setf tokenizer (make-instance 'tokenizer :stream stream)))))

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

(defclass token-parser (parser)
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

(defstruct simple-block (associated-token) (value))

(define-serialize-method (simple-block stream)
  (let ((ending-token
         (typecase (simple-block-associated-token simple-block)
           (left-parenthesis-token (make-right-parenthesis-token))
           (left-square-bracket-token (make-right-square-bracket-token))
           (left-curly-bracket-token (make-right-curly-bracket-token)))))
    (serialize (simple-block-associated-token simple-block) stream)
    (serialize-tokens (simple-block-value simple-block) stream)
    (serialize ending-token stream)))

(defun parse-stylesheet (parser)
  (consume-list-of-rules parser))

(defgeneric parse-rules (source)
  (:method ((string string))
   (with-input-from-string (stream string)
     (parse-rules stream)))
  (:method ((stream stream))
   (let ((parser (make-instance 'parser :stream stream)))
     (parse-rules parser)))
  (:method ((parser parser))
   (consume-list-of-rules parser)))

(defun parse-rule (parser)
  (loop while (whitespace-token-p (next-input-token parser))
        do (consume-next-input-token parser))
  (when (null (next-input-token parser))
    (error 'syntax-error))
  (prog1
      (if (at-keyword-token-p (next-input-token parser))
          (consume-at-rule parser)
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

(defgeneric parse-declarations (soruce)
  (:method ((string string))
   (with-input-from-string (stream string)
     (parse-declarations stream)))
  (:method ((stream stream))
   (let ((parser (make-instance 'parser :stream stream)))
     (parse-declarations parser)))
  (:method ((block simple-block))
   (let ((parser (make-instance 'token-parser :tokens (simple-block-value block))))
     (parse-declarations parser)))
  (:method ((parser parser))
   (consume-list-of-declarations parser)))

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
              ((null token) (return (reverse rules)))
              ((at-keyword-token-p token)
               (reconsume-current-input-token parser)
               (push (consume-at-rule parser) rules))
              (t (reconsume-current-input-token parser)
                 (push (consume-qualified-rule parser) rules)))
          finally (return (reverse rules)))))

(defun consume-at-rule (parser)
  (let* ((at-keyword-token (consume-next-input-token parser))
         (name (at-keyword-token-value at-keyword-token))
         (rule (make-instance 'at-rule :name name)))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((semicolon-token-p token) (return rule))
              ((null token) (return rule))
              ((left-curly-bracket-token-p token)
               (setf (rule-block rule) (consume-simple-block parser))
               (return rule))
              ((and (simple-block-p token)
                    (left-curly-bracket-token-p
                     (simple-block-associated-token token)))
               (setf (rule-block rule) token)
               (return rule))
              (t (reconsume-current-input-token parser)
                 (appendf (rule-prelude rule)
                          (list (consume-component-value parser))))))))

(defun consume-qualified-rule (parser)
  (let ((rule (make-instance 'qualified-rule)))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((null token) (return))
              ((left-curly-bracket-token-p token)
               (setf (rule-block rule) (consume-simple-block parser))
               (reversef (rule-prelude rule))
               (return rule))
              ((and (simple-block-p token)
                    (left-curly-bracket-token-p
                     (simple-block-associated-token token)))
               (setf (rule-block rule) token)
               (reversef (rule-prelude rule))
               (return rule))
              (t (reconsume-current-input-token parser)
                 (push (consume-component-value parser) (rule-prelude rule)))))))

(defun consume-list-of-declarations (parser)
  (let ((declarations '()))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((or (whitespace-token-p token)
                   (semicolon-token-p token)))
              ((null token) (return (reverse declarations)))
              ((at-keyword-token-p token)
               (reconsume-current-input-token parser)
               (push (consume-at-rule parser) declarations))
              ((ident-token-p token)
               (let ((temporary-list `(,token)))
                 (loop while (and (not (semicolon-token-p (next-input-token parser)))
                                  (not (null (next-input-token parser))))
                       do (push (consume-component-value parser) temporary-list))
                 (push
                  (consume-declaration
                   (make-instance 'token-parser
                                  :tokens (reverse temporary-list)))
                  declarations)))
              (t (reconsume-current-input-token parser)
                 (loop while (and (not (semicolon-token-p (next-input-token parser)))
                                  (not (null (next-input-token parser))))
                       do (consume-component-value parser)))))))

(defun consume-declaration (parser)
  (let* ((ident-token (consume-next-input-token parser))
         (name (ident-token-value ident-token))
         (value '())
         (important))
    (loop while (whitespace-token-p (next-input-token parser))
          do (consume-next-input-token parser))
    (if (not (colon-token-p (next-input-token parser)))
        (return-from consume-declaration)
      (consume-next-input-token parser))
    (loop while (whitespace-token-p (next-input-token parser))
          do (consume-next-input-token parser))
    (loop while (next-input-token parser)
          do (appendf value (list (consume-component-value parser))))
    (let ((last-two-tokens (reverse
                            (take 2 (reverse (remove 'whitespace-token value
                                                     :key 'type-of))))))
      (when (and (delim-token-p (first last-two-tokens))
                 (eq #\! (delim-token-value (first last-two-tokens)))
                 (ident-token-p (second last-two-tokens))
                 (equal "important" (ident-token-value (second last-two-tokens))))
        (progn
          (setf value (remove (first last-two-tokens) value))
          (setf value (remove (second last-two-tokens) value))
          (setf important t)))
      (loop for token in (reverse value)
            while (whitespace-token-p token)
            do (setf value (remove token value))))
    (make-instance 'declaration
                   :name name
                   :value value
                   :important important)))

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
    (flet ((return-simple-block ()
             (reversef (simple-block-value simple-block))
             (return-from consume-simple-block simple-block)))
      (loop for token = (consume-next-input-token parser)
            do (cond
                ((typep token ending-token-type) (return-simple-block))
                ((null token) (return-simple-block))
                (t (reconsume-current-input-token parser)
                   (push (consume-component-value parser)
                         (simple-block-value simple-block))))))))

(defun consume-function (parser)
  (let* ((function-token (current-input-token parser))
         (name (function-token-value function-token))
         (function (make-instance 'function :name name :value '())))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((right-parenthesis-token-p token) (return function))
              ((null token) (return function))
              (t (reconsume-current-input-token parser)
                 (appendf (function-value function)
                          (list (consume-component-value parser))))))))
