(in-package :css)

(define-condition syntax-error (error) ())

(defclass parser ()
  ((stream
    :initarg :stream
    :initform nil)
   (tokenizer
    :initarg tokenizer
    :initform nil)
   (current-input-token
    :initform nil
    :reader current-input-token)
   (next-input-token
    :initform nil)))

(defun next-input-token (parser)
  (with-slots (tokenizer next-input-token) parser
    (or next-input-token
        (when-let ((token (consume-token tokenizer)))
          (setf next-input-token token)))))

(defun consume-next-input-token (parser)
  (with-slots (tokenizer current-input-token next-input-token) parser
    (let ((token (if next-input-token
                     next-input-token
                   (consume-token tokenizer))))
      (setf next-input-token nil
            current-input-token token))))

(defun reconsume-current-input-token (parser)
  (with-slots (current-input-token next-input-token) parser
    (when current-input-token
      (setf next-input-token current-input-token))))

(defun parse-stylesheet ()
  )

(defun parse-rule ()
  )

(defun parse-declaration (parser)
  (loop for token = (next-input-token parser)
        do (cond
            ((whitespace-token-p token) (consume-next-input-token parser))
            ((not (ident-token-p token)) (error 'syntax-error))
            (t (or (consume-declaration parser) (error 'syntax-error))))))

(defun consume-declaration (parser)
  (let ((name (consume-next-input-token parser))
        (value '()))
    (loop while (whitespace-token-p (next-input-token parser))
          do (consume-next-input-token parser))
    (if (colon-token-p (next-input-token parser))
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
        (block '()))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((typep token ending-token-type) (return block))
              ((null token) (return block))
              (t (reconsume-current-input-token parser)
                 (appendf block (list (consume-component-value parser))))))))

(defun consume-function (parser)
  (let* ((name (current-input-token parser))
         (function `(:function ,name)))
    (loop for token = (consume-next-input-token parser)
          do (cond
              ((right-parenthesis-token-p token) (return function))
              ((null token) (return function))
              (t (reconsume-current-input-token parser)
                 (appendf function (list (consume-component-value parser))))))))
